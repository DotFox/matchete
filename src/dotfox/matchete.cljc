(ns dotfox.matchete
  (:refer-clojure :exclude [compile])
  (:require [clojure.core.async :as async]))

(defn- make-channel-form [form]
  (case (count form)
    1 `(async/chan 1024 (distinct))
    2 (case (second form)
        1 form
        (concat form '((distinct))))
    form))

(defmacro with-chan [[ch form] & body]
  `(let [~ch ~(make-channel-form form)]
     ~@body
     ~ch))

(defmulti compile* (fn [pattern]
                     (cond
                       (sequential? pattern) (first pattern)
                       ((some-fn ifn? fn?) pattern) :pred)))

(defmethod compile* := [[_ value]]
  (fn [_registry bindings data]
    (with-chan [ch (async/chan 1)]
      (if (= value data)
        (async/go
          (async/>! ch bindings)
          (async/close! ch))
        (async/close! ch)))))

(defmethod compile* :? [[_ binding]]
  (fn [_registry bindings data]
    (with-chan [ch (async/chan 1)]
      (async/go
        (cond
          (not (contains? bindings binding))
          (async/>! ch (assoc bindings binding data))

          (= data (get bindings binding))
          (async/>! ch bindings))
        (async/close! ch)))))

(defmethod compile* :! [[_ binding]]
  (fn [_registry bindings data]
    (with-chan [ch (async/chan 1)]
      (async/go
        (async/>! ch (update bindings binding (fnil conj []) data))
        (async/close! ch)))))

(defmethod compile* :or [[_ & patterns]]
  (let [matchers (doall (map compile* patterns))]
    (fn [registry bindings data]
      (with-chan [ch (async/chan)]
        (async/go-loop [ports (set (map #(% registry bindings data) matchers))]
          (if-let [ports' (seq ports)]
            (let [[res port] (async/alts! ports')]
              (if res
                (do (async/>! ch res)
                    (recur ports))
                (recur (disj ports port))))
            (async/close! ch)))))))

(defmethod compile* :orn [[_ {:keys [key]} & binding-forms]]
  (let [matchers (into {} (map (fn [[value pattern]]
                                 [value (compile* pattern)]))
                       binding-forms)]
    (fn [registry bindings data]
      (with-chan [ch (async/chan)]
        (let [binding-ports (into {} (map (fn [[value matcher]]
                                            [(matcher registry bindings data) value]))
                                  matchers)]
          (async/go-loop [ports (set (keys binding-ports))]
            (if-let [ports' (seq ports)]
              (let [[res port] (async/alts! ports')]
                (if res
                  (do (async/>! ch (assoc res key (get binding-ports port)))
                      (recur ports))
                  (recur (disj ports port))))
              (async/close! ch))))))))

(defn chain [[matcher & matchers]]
  (if matcher
    (let [post-fn (chain matchers)]
      (fn [registry bindings data]
        (with-chan [ch (async/chan)]
          (let [ch' (matcher registry bindings data)]
            (async/go-loop []
              (if-let [res (async/<! ch')]
                (let [ch'' (post-fn registry res data)]
                  (loop []
                    (when-let [res (async/<! ch'')]
                      (async/>! ch res)
                      (recur)))
                  (recur))
                (async/close! ch)))))))
    (fn [_registry bindings _data]
      (with-chan [ch (async/chan)]
        (async/go
          (async/>! ch bindings)
          (async/close! ch))))))

(defmethod compile* :and [[_ & patterns]]
  (let [matchers (doall (map compile* patterns))
        chain (chain matchers)]
    (fn [registry bindings data]
      (with-chan [ch (async/chan)]
        (let [in-ch (chain registry bindings data)]
          (async/go-loop []
            (if-let [res (async/<! in-ch)]
              (do (async/>! ch res)
                  (recur))
              (async/close! ch))))))))

(defmethod compile* :not [[_ pattern]]
  (let [matcher (compile* pattern)]
    (fn [registry bindings data]
      (with-chan [ch (async/chan 1)]
        (let [in-ch (matcher registry bindings data)]
          (async/go
            (if (async/<! in-ch)
              (async/close! ch)
              (do (async/>! ch bindings)
                  (async/close! ch)))))))))

(defmethod compile* :enum [[_ & values]]
  (compile* (vec (cons :or (map #(vector := %) values)))))

(defmethod compile* :pred [predicate]
  (fn [_ bindings data]
    (with-chan [ch (async/chan 1)]
      (if (predicate data)
        (async/go
          (async/>! ch bindings)
          (async/close! ch))
        (async/close! ch)))))

(defmethod compile* :ref [[_ ref]]
  (fn [registry bindings data]
    ((get registry ref) registry bindings data)))

(defn sequential-chain [matcher]
  (fn [registry bindings data]
    (if (empty? data)
      (with-chan [ch (async/chan)]
        (async/go
          (async/>! ch bindings)
          (async/close! ch)))
      (with-chan [ch (async/chan)]
        (let [ch' (matcher registry bindings (first data))]
          (async/go-loop []
            (if-let [res (async/<! ch')]
              (let [post-fn (sequential-chain matcher)
                    ch'' (post-fn registry res (rest data))]
                (loop []
                  (when-let [res (async/<! ch'')]
                    (async/>! ch res)
                    (recur)))
                (recur))
              (async/close! ch))))))))

(defn- compile-homogeneous-sequence [pred pattern]
  (let [matcher (compile* pattern)
        chain (sequential-chain matcher)]
    (fn [registry bindings data]
      (with-chan [ch (async/chan)]
        (if (pred data)
          (let [in-ch (chain registry bindings data)]
            (async/go-loop []
              (if-let [res (async/<! in-ch)]
                (do (async/>! ch res)
                    (recur))
                (async/close! ch))))
          (async/close! ch))))))

(defmethod compile* :sequential [[_ pattern]]
  (compile-homogeneous-sequence sequential? pattern))

(defmethod compile* :vector [[_ pattern]]
  (compile-homogeneous-sequence vector? pattern))

(defn cat-chain [[matcher & matchers]]
  (if matcher
    (let [post-fn (cat-chain matchers)]
      (fn [registry bindings data]
        (with-chan [ch (async/chan)]
          (if (empty? data)
            (async/close! ch)
            (let [ch' (matcher registry bindings (first data))]
              (async/go-loop []
                (if-let [res (async/<! ch')]
                  (let [ch'' (post-fn registry res (rest data))]
                    (loop []
                      (when-let [res (async/<! ch'')]
                        (async/>! ch res)
                        (recur)))
                    (recur))
                  (async/close! ch))))))))
    (fn [_registry bindings _data]
      (with-chan [ch (async/chan 1)]
        (async/go
          (async/>! ch bindings)
          (async/close! ch))))))

(defmethod compile* :tuple [[_ & patterns]]
  (let [matchers (doall (map compile* patterns))
        chain (cat-chain matchers)]
    (fn [registry bindings data]
      (with-chan [ch (async/chan)]
        (if (= (count matchers)
               (count data))
          (let [in-ch (chain registry bindings data)]
            (async/go-loop []
              (if-let [res (async/<! in-ch)]
                (do (async/>! ch res)
                    (recur))
                (async/close! ch))))
          (async/close! ch))))))

(defmethod compile* :cat [[_ & patterns]]
  (let [matchers (doall (map compile* patterns))
        chain (cat-chain matchers)]
    (fn [registry bindings data]
      (with-chan [ch (async/chan)]
        (if (sequential? data)
          (let [in-ch (chain registry bindings data)]
            (async/go-loop []
              (if-let [res (async/<! in-ch)]
                (do (async/>! ch res)
                    (recur))
                (async/close! ch))))
          (async/close! ch))))))

(defn repeat-chain
  ([matcher]
   (repeat-chain matcher 0 ##Inf 0))
  ([matcher min]
   (repeat-chain matcher min ##Inf 0))
  ([matcher min max]
   (repeat-chain matcher min max 0))
  ([matcher min max current]
   (fn [registry bindings data]
     (with-chan [ch (async/chan)]
       (cond
         (empty? data)
         (async/go
           (if (>= current min)
             (async/>! ch bindings)
             (async/close! ch)))

         (> current max)
         (async/close! ch)

         :else
         (let [post-fn (repeat-chain matcher min max (inc current))
               ch' (matcher registry bindings (first data))]
           (async/go-loop []
             (if-let [res (async/<! ch')]
               (let [ch'' (post-fn bindings res (rest data))]
                 (loop []
                   (when-let [res (async/<! ch'')]
                     (async/>! ch res)
                     (recur))))
               (async/close! ch)))))))))

(defmethod compile* :* [[_ pattern]]
  (let [matcher (compile* pattern)
        chain (repeat-chain matcher)]
    (fn [registry bindings data]
      (with-chan [ch (async/chan)]
        (if (sequential? data)
          (let [in-ch (chain registry bindings data)]
            (async/go-loop []
              (if-let [res (async/<! in-ch)]
                (do (async/>! ch res)
                    (recur))
                (async/close! ch))))
          (async/close! ch))))))

(defmethod compile* :+ [[_ pattern]]
  (let [matcher (compile* pattern)
        chain (repeat-chain matcher 1)]
    (fn [registry bindings data]
      (with-chan [ch (async/chan)]
        (if (sequential? data)
          (let [in-ch (chain registry bindings data)]
            (async/go-loop []
              (if-let [res (async/<! in-ch)]
                (do (async/>! ch res)
                    (recur))
                (async/close! ch))))
          (async/close! ch))))))

(defmethod compile* :search [[_ pattern]]
  (let [matcher (compile* pattern)]
    (fn [registry bindings data]
      (with-chan [ch (async/chan)]
        (async/go-loop [ports (set (map #(matcher registry bindings %) data))]
          (if-let [ports' (seq ports)]
            (let [[res port] (async/alts! ports')]
              (if res
                (do (async/>! ch res)
                    (recur ports))
                (recur (disj ports port))))
            (async/close! ch)))))))

(defn map-chain [patterns]
  (if-let [[[k-pattern v-pattern] & patterns] (seq patterns)]
    (let [k-matcher (compile* k-pattern)
          v-matcher (compile* v-pattern)
          chain (map-chain patterns)]
      (fn [registry bindings data]
        (with-chan [ch (async/chan)]
          (let [k-chain (fn [k]
                          (with-chan [ch' (async/chan)]
                            (let [k-port (k-matcher registry bindings k)]
                              (async/go-loop []
                                (if-let [res (async/<! k-port)]
                                  (let [v-port (v-matcher registry res (get data k))]
                                    (loop []
                                      (when-let [res (async/<! v-port)]
                                        (let [port (chain registry res (dissoc data k))]
                                          (loop []
                                            (when-let [res (async/<! port)]
                                              (async/>! ch' res)
                                              (recur))))
                                        (recur)))
                                    (recur))
                                  (async/close! ch'))))))]
            (if-let [keys (keys data)]
              (async/go-loop [ports (into #{} (map k-chain) keys)]
                (if-let [ports' (seq ports)]
                  (let [[res port] (async/alts! ports')]
                    (if res
                      (do (async/>! ch res)
                          (recur ports))
                      (recur (disj ports port))))
                  (async/close! ch)))
              (async/close! ch))))))
    (fn [_registry bindings _data]
      (with-chan [ch (async/chan 1)]
        (async/go
          (async/>! ch bindings)
          (async/close! ch))))))

(defmethod compile* :map [[_ & kv-patterns]]
  (let [matcher (map-chain kv-patterns)]
    (fn [registry bindings data]
      (with-chan [ch (async/chan)]
        (if (map? data)
          (async/go-loop [port (matcher registry bindings data)]
            (if-let [res (async/<! port)]
              (do
                (async/>! ch res)
                (recur port))
              (async/close! ch)))
          (async/close! ch))))))

(defn set-chain [matchers]
  (if-let [[matcher & matchers] (seq matchers)]
    (let [chain (set-chain matchers)]
      (fn [registry bindings data]
        (with-chan [ch (async/chan)]
          (let [ports (into #{}
                            (map (fn [el]
                                   (with-chan [ch (async/chan)]
                                     (async/go-loop [port (matcher registry bindings el)]
                                       (if-let [res (async/<! port)]
                                         (do
                                           (loop [port (chain registry res (disj data el))]
                                             (when-let [res (async/<! port)]
                                               (async/>! ch res)
                                               (recur port)))
                                           (recur port))
                                         (async/close! ch))))))
                            data)]
            (async/go-loop [ports ports]
              (if-let [ports' (seq ports)]
                (let [[res port] (async/alts! ports')]
                  (if res
                    (do (async/>! ch res)
                        (recur ports))
                    (recur (disj ports port))))
                (async/close! ch)))))))
    (fn [_registry bindings _data]
      (with-chan [ch (async/chan 1)]
        (async/go
          (async/>! ch bindings)
          (async/close! ch))))))

(defmethod compile* :set [[_ & v-patterns]]
  (let [matcher (set-chain (doall (map compile* v-patterns)))]
    (fn [registry bindings data]
      (with-chan [ch (async/chan)]
        (if (set? data)
          (async/go-loop [port (matcher registry bindings data)]
            (if-let [res (async/<! port)]
              (do
                (async/>! ch res)
                (recur port))
              (async/close! ch)))
          (async/close! ch))))))

(defmethod compile* :set-of [[_ v-pattern]]
  (let [matcher (compile* [:sequential v-pattern])]
    (fn [registry bindings data]
      (with-chan [ch (async/chan)]
        (if (set? data)
          (async/go-loop [port (matcher registry bindings (seq data))]
            (if-let [res (async/<! port)]
              (do (async/>! ch res)
                  (recur port))
              (async/close! ch)))
          (async/close! ch))))))

(defmethod compile* :guard [[_ guard-fn]]
  (fn [_registry bindings data]
    (with-chan [ch (async/chan)]
      (async/go
        (let [results (guard-fn bindings data)
              results (cond
                        (map? results) (list results)
                        (sequential? results) (seq results)
                        (nil? results) ())]
          (doseq [result results]
            (async/>! ch result))
          (async/close! ch))))))

(defn compile [pattern]
  (let [matcher (compile* pattern)]
    (fn m
      ([data]
       (m {} {} data))
      ([bindings data]
       (m {} bindings data))
      ([registry bindings data]
       (matcher registry bindings data)))))

(comment

  (let [plus-guard (fn f
                     ([key] (f key 0))
                     ([key k]
                      (fn [bindings data]
                        (if-let [binding (and (contains? bindings key)
                                              (get bindings key))]
                          (when (= data (+ binding k))
                            bindings)
                          (assoc bindings key (- data k))))))
        matcher (compile [:orn {:key :hand}
                          [:strait-flush [:set
                                          [:tuple [:? :suit] [:guard (plus-guard :rank)]]
                                          [:tuple [:? :suit] [:guard (plus-guard :rank 1)]]
                                          [:tuple [:? :suit] [:guard (plus-guard :rank 2)]]
                                          [:tuple [:? :suit] [:guard (plus-guard :rank 3)]]
                                          [:tuple [:? :suit] [:guard (plus-guard :rank 4)]]]]
                          [:strait [:set
                                    [:tuple any? [:guard (plus-guard :rank)]]
                                    [:tuple any? [:guard (plus-guard :rank 1)]]
                                    [:tuple any? [:guard (plus-guard :rank 2)]]
                                    [:tuple any? [:guard (plus-guard :rank 3)]]
                                    [:tuple any? [:guard (plus-guard :rank 4)]]]]
                          [:royal-flush [:set
                                         [:tuple [:? :suit] [:= 10]]
                                         [:tuple [:? :suit] [:= 11]]
                                         [:tuple [:? :suit] [:= 12]]
                                         [:tuple [:? :suit] [:= 13]]
                                         [:tuple [:? :suit] [:= 14]]]]
                          [:four-of-kind [:set
                                          any?
                                          [:tuple any? [:? :rank]]
                                          [:tuple any? [:? :rank]]
                                          [:tuple any? [:? :rank]]
                                          [:tuple any? [:? :rank]]]]
                          [:three-of-kind [:set
                                           any?
                                           any?
                                           [:tuple any? [:? :rank]]
                                           [:tuple any? [:? :rank]]
                                           [:tuple any? [:? :rank]]]]
                          [:full-house [:set
                                        [:tuple any? [:? :full]]
                                        [:tuple any? [:? :full]]
                                        [:tuple any? [:? :full]]
                                        [:tuple any? [:? :house]]
                                        [:tuple any? [:? :house]]]]
                          [:pair [:set
                                  any?
                                  any?
                                  any?
                                  [:tuple any? [:? :rank]]
                                  [:tuple any? [:? :rank]]]]
                          [:two-pairs [:set
                                       any?
                                       [:tuple any? [:? :rank-1]]
                                       [:tuple any? [:? :rank-1]]
                                       [:tuple any? [:? :rank-2]]
                                       [:tuple any? [:? :rank-2]]]]
                          [:flush [:set-of [:tuple [:? :suit] any?]]]
                          [:highest-card [:set-of [:tuple any? [:guard (fn [{:keys [rank] :as bindings} data]
                                                                         (if (some? rank)
                                                                           (if (>= data rank)
                                                                             (assoc bindings :rank data)
                                                                             bindings)
                                                                           {:rank data}))]]]]])
        samples [#{[:♠ 10] [:♠ 11] [:♠ 12] [:♠ 13] [:♠ 14]}
                 #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 5]}
                 #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 7]}
                 #{[:♠ 5] [:♠ 6] [:♠ 7] [:♠ 13] [:♠ 9]}
                 #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 8]}
                 #{[:♠ 5] [:♦ 10] [:♠ 7] [:♣ 5] [:♥ 10]}
                 #{[:♠ 5] [:♠ 6] [:♠ 7] [:♠ 8] [:♠ 9]}
                 #{[:♠ 5] [:♣ 6] [:♠ 7] [:♠ 8] [:♠ 9]}]
        sample (rand-nth samples)]
    (time
     (dotimes [_ 100]
       (async/<!! (async/go-loop [acc [] port (matcher sample)]
                    (if-let [res (async/<! port)]
                      (recur (conj acc res) port)
                      acc)))))
    (async/<!! (async/go-loop [acc [] port (matcher sample)]
                 (if-let [res (async/<! port)]
                   (recur (conj acc res) port)
                   acc))))

  (async/<!! ((compile [:set [:= 1]]) #{1}))

  (let [ch ((compile [:and [:ref ::number] [:? :x]]) {::number (compile number?)} {} "")]
    (async/<!! ch))

  (async/<!! ((compile [:enum "foo" "bar" "baz"]) "baz"))

  (async/<!! ((compile [:sequential [:and int? [:? :x]]]) (list 1 1 1)))

  (async/<!! ((compile [:not string?]) ""))

  (async/<!! ((compile [:tuple int? int?]) [1 1]))

  (async/<!! ((compile [:cat [:! :x] [:! :x] [:! :x]]) '("" 0 2.3)))

  (async/<!! ((compile [:* int?]) []))

  (async/<!! ((compile [:+ int?]) [1]))

  (let [matcher (compile [:set [:= 1] [:= 6] [:? :x]])]
    (time
     (dotimes [_ 1000]
       (let [ch (matcher #{1 2 3 4 5 6})]
         (async/<!! (async/go-loop [acc [] ch ch]
                      (if-let [res (async/<! ch)]
                        (recur (conj acc res) ch)
                        acc)))))))

  ;; [:and [:? :x] [:? :y] [:= 42]]
  ;;          |       |       |
  ;;        ( r )   ( r )   ( r )
  ;;

  )
