(ns dotfox.matchete
  (:require [clojure.core.async :as async]
            [clojure.set :as set]))

;; === utils ===

(defn wrap-matcher [matcher]
  (fn f
    ([data] (f {} data))
    ([bindings data]
     (matcher bindings data))))

(defn matcher->seq-matcher [matcher]
  (fn [bindings data]
    (async/pipe (matcher bindings (first data))
                (async/chan 1024 (comp (distinct) (map (fn [res] [1 res])))))))

(defn seq-matcher->top-level-seq-matcher [matcher]
  (fn [bindings data]
    (if (sequential? data)
      (let [size (count data)]
        (async/pipe (matcher bindings data)
                    (async/chan 1024 (comp (filter (fn [[consumed _]] (= consumed size)))
                                           (map (fn [[_ res]] res))))))
      (async/to-chan! []))))

(defn map-vals [m f]
  (into {} (map (juxt first (comp f second))) m))

(defn distinct-chan
  ([] (distinct-chan 1024))
  ([buf-or-n] (distinct-chan buf-or-n nil))
  ([buf-or-n xform] (distinct-chan buf-or-n xform nil))
  ([buf-or-n xform ex-handler]
   (async/chan buf-or-n (if (some? xform) (comp (distinct) xform) (distinct)) ex-handler)))

(defn combine-matchers [matchers]
  (if (seq matchers)
     (let [[matcher & matchers] matchers
           continuation (combine-matchers matchers)]
       (fn [bindings data]
         (let [out-ch (distinct-chan)
               port (matcher bindings data)]
           (async/go-loop [ports []]
             (if-let [res (async/<! port)]
               (recur (conj ports (continuation res data)))
               (if (seq ports)
                 (async/pipe (async/merge (vec ports)) out-ch)
                 (async/close! out-ch))))
           out-ch)))
     (fn [bindings _data]
       (async/to-chan! [bindings]))))

(defn consuming-sequence-matcher [matchers]
  (if (seq matchers)
    (let [[matcher & matchers] matchers]
      (fn [bindings data]
        (let [out-ch (distinct-chan)]
          (let [continuation (consuming-sequence-matcher matchers)
                port (matcher bindings data)]
            (async/go-loop [ports []]
              (if-let [res (async/<! port)]
                (let [[consumed res] res]
                  (recur (conj ports
                               (async/pipe (continuation res (drop consumed data))
                                           (distinct-chan 1024
                                                          (map (fn [[consumed' res]]
                                                                 [(+ consumed consumed')
                                                                  res])))))))
                (if (seq ports)
                  (async/pipe (async/merge ports) out-ch)
                  (async/close! out-ch)))))
          out-ch)))
    (fn [bindings _data]
      (async/to-chan! [0 bindings]))))

(defn repeat-matcher* [min max counter matcher]
  (letfn [(match-and-continue [bindings data]
            (let [out-ch (distinct-chan)
                  continuation (repeat-matcher* min max (inc counter) matcher)
                  port (matcher bindings data)]
              (async/go-loop [ports []]
                (if-let [res (async/<! port)]
                  (let [[consumed res] res]
                    (recur (conj ports
                                 (async/pipe (continuation res (drop consumed data))
                                             (distinct-chan 1024
                                                            (map (fn [[consumed' res]]
                                                                   [(+ consumed consumed')
                                                                    res])))))))
                  (if (seq ports)
                    (async/pipe (async/merge ports) out-ch)
                    (async/close! out-ch))))
              out-ch))
          (success-and-close [bindings]
            (async/to-chan [0 bindings]))]
    (cond
      (< counter min)
      (fn [bindings data]
        (if (seq data)
          (match-and-continue bindings data)
          (async/to-chan! [])))

      (and (>= counter min)
           (< counter max))
      (fn [bindings data]
        (let [out-ch-1 (success-and-close bindings)]
          (if (seq data)
            (let [out-ch-2 (match-and-continue bindings data)]
              (async/merge [out-ch-1 out-ch-2]))
            out-ch-1)))

      (>= counter max)
      (fn [bindings _data]
        (success-and-close bindings)))))

(defn pattern? [registry pattern]
  (or (and (vector? pattern) (contains? @registry (first pattern)))
      (contains? @registry pattern)))

;; === matchers ===

(declare matcher seq-matcher)

(def ^:dynamic *seq-context* false)

;; --- logic-matchers ---

(defn lvar-matcher [_ binding]
  (letfn [(lvar-matcher* [bindings data]
            (cond
              (and (contains? bindings binding)
                   (= data (get bindings binding)))
              (async/to-chan! [bindings])

              (not (contains? bindings binding))
              (async/to-chan! [(assoc bindings binding data)])

              :else (async/to-chan! [])))]
    (if *seq-context*
      (matcher->seq-matcher lvar-matcher*)
      lvar-matcher*)))

(defn mvar-matcher [_ binding]
  (letfn [(mvar-matcher* [bindings data]
            (async/to-chan! [(update bindings binding (fnil conj []) data)]))]
    (if *seq-context*
      (matcher->seq-matcher mvar-matcher*)
      mvar-matcher*)))

(defn fresh-matcher [registry {:keys [merge-fn]} pattern]
  (let [merge-fn (if *seq-context*
                   (fn [prev [consumed new]]
                     [consumed (merge-fn prev new)])
                   merge-fn)
        matcher (matcher registry pattern)]
    (fn [bindings data]
      (async/pipe (matcher bindings data)
                  (distinct-chan 1024 (keep merge-fn))))))

(defn interceptor-matcher [registry {:keys [pre post]
                                     :or {pre identity
                                          post identity}}
                           pattern]
  (let [post (if *seq-context*
               (fn [[consumed res]]
                 [consumed (post res)])
               post)
        matcher (matcher registry pattern)]
    (fn [bindings data]
      (async/pipe (matcher (pre bindings) data)
                  (distinct-chan (keep post))))))

(defn functional-matcher [_registry f]
  (fn [bindings data]
    (if-let [res (f bindings data)]
      (async/pipe (async/to-chan! res) (distinct-chan))
      (async/to-chan! []))))

;; --- base-matchers ---

(defn and-matcher [registry & patterns]
  (if *seq-context*
    (let [seq-matcher (matcher registry (first patterns))
          matcher (binding [*seq-context* false]
                    (combine-matchers (mapv (partial matcher registry) (rest patterns))))]
      (fn [bindings data]
        (let [port (seq-matcher bindings data)
              out-ch (distinct-chan)]
          (async/go-loop [ports []]
            (if-let [res (async/<! port)]
              (let [[consumed res] res
                    port (matcher res (take consumed data))]
                (recur (conj ports
                             (async/pipe port
                                         (distinct-chan 1024 (map (fn [res]
                                                                    [consumed res])))))))
              (async/pipe (async/merge ports) out-ch)))
          out-ch)))
    (combine-matchers (mapv (partial matcher registry) patterns))))

(defn or-matcher [registry & patterns]
  (let [matchers (mapv (partial matcher registry) patterns)]
    (fn [bindings data]
      (async/pipe (async/merge (mapv #(% bindings data) matchers)) (distinct-chan)))))

(defn orn-matcher [registry opts & patterns]
  (let [key (if (map? opts) (:key opts) opts)
        matchers (map (juxt first #(matcher registry (second %))) patterns)]
    (fn [bindings data]
      (async/pipe
       (async/merge (mapv
                     (fn [[name matcher]]
                       (async/pipe
                        (matcher bindings data)
                        (distinct-chan 1024
                                       (map (fn [res]
                                              (if (vector? res)
                                                (update-in res [1] assoc key name)
                                                (assoc res key name)))))))
                     matchers))
       (distinct-chan)))))

(defn not-matcher [registry pattern]
  (let [matcher (matcher registry pattern)]
    (fn [bindings data]
      (let [port (matcher bindings data)
            out-ch (async/chan 1)]
        (async/go
          (if (async/<! port)
            (async/close! out-ch)
            (do (async/>! out-ch bindings)
                (async/close! out-ch))))
        out-ch))))

(defn maybe-matcher [registry pattern]
  (let [matcher (matcher registry pattern)
        maybe-matcher* (fn [bindings data]
                         (if (nil? data)
                           (async/to-chan! [bindings])
                           (matcher bindings data)))]
    (if *seq-context*
      (matcher->seq-matcher maybe-matcher*)
      maybe-matcher*)))

(defn enum-matcher [_ & values]
  (let [values (set values)
        enum-matcher* (fn [bindings data]
                        (async/to-chan! (when (contains? values data) [bindings])))]
    (if *seq-context*
      (matcher->seq-matcher enum-matcher*)
      enum-matcher*)))

(defn multi-matcher [registry {:keys [dispatch]} & patterns]
  (let [matchers (into {}
                       (map (fn [[dispatch-value pattern]]
                              [dispatch-value (matcher registry pattern)]))
                       patterns)
        multi-matcher* (fn [bindings data]
                         (let [dispatch-value (dispatch data)]
                           (if-let [matcher (matchers dispatch-value (:default matchers))]
                             (matcher bindings data)
                             (async/to-chan! []))))]
    (if *seq-context*
      (matcher->seq-matcher multi-matcher*)
      multi-matcher*)))

(defn regex-matcher [_registry regex]
  (let [regex (cond
                (string? regex)
                (re-pattern regex)

                (instance? #?(:clj java.util.regex.Pattern
                              :cljs js/RegExp)
                           regex)
                regex

                :else
                (throw (ex-info "Invalid regex form" {:form regex})))
        regex-matcher* (fn [bindings data]
                         (async/to-chan! (when (and (string? data) (re-matches regex data)) [bindings])))]
    (if *seq-context*
      (matcher->seq-matcher regex-matcher*)
      regex-matcher*)))

;; --- collection-matchers ---

(defn sequence-matcher [matchers]
  (if (seq matchers)
    (let [[matcher & matchers] matchers
          continuation (sequence-matcher matchers)]
      (fn [bindings data]
        (let [out-ch (distinct-chan)
              port (matcher bindings (first data))]
          (async/go-loop [ports []]
            (if-let [res (async/<! port)]
              (recur (conj ports (continuation res (rest data))))
              (if (seq ports)
                (async/pipe (async/merge (vec ports)) out-ch)
                (async/close! out-ch))))
          out-ch)))
    (fn [bindings _data]
      (async/to-chan! [bindings]))))

(defn tuple-matcher [registry & patterns]
  (let [size (count patterns)
        matcher (binding [*seq-context* false]
                  (sequence-matcher (mapv (partial matcher registry) patterns)))]
    (if *seq-context*
      (fn [bindings data]
        (if (and (sequential? data) (>= (count data) size))
          (async/pipe (matcher bindings data)
                      (async/chan 1024 (map (fn [res] [size res]))))
          (async/to-chan! [])))
      (fn [bindings data]
        (if (and (sequential? data) (= (count data) size))
          (matcher bindings data)
          (async/to-chan! []))))))

(defn fixed-set-matcher [constants continuation]
  (fn [bindings data]
    (if (set/subset? constants data)
      (continuation bindings (set/difference data constants))
      (async/to-chan! []))))

(defn dynamic-set-matcher [registry patterns]
  (if (seq patterns)
    (let [matcher (matcher registry (first patterns))
          continuation (dynamic-set-matcher registry (rest patterns))]
      (fn [bindings data]
        (let [ports (mapv (fn [el]
                            (let [matcher (fn [bindings data]
                                            (let [port (matcher bindings el)
                                                  out-ch (distinct-chan)]
                                              (async/go-loop [ports []]
                                                (if-let [res (async/<! port)]
                                                  (recur (conj ports (continuation res (disj data el))))
                                                  (if (seq ports)
                                                    (async/pipe (async/merge ports)
                                                                out-ch)
                                                    (async/close! out-ch))))
                                              out-ch))]
                              (matcher bindings data)))
                          data)]
          (async/pipe (async/merge ports) (distinct-chan)))))
    (fn [bindings _data]
      (async/to-chan! [bindings]))))

(defn set-matcher [registry & patterns]
  (let [min-size (count patterns)
        {constants false
         patterns true} (group-by (partial pattern? registry) patterns)
        dynamic-set-matcher (binding [*seq-context* false]
                              (dynamic-set-matcher registry patterns))
        fixed-set-matcher (binding [*seq-context* false]
                            (fixed-set-matcher (set constants) dynamic-set-matcher))
        set-matcher* (fn [bindings data]
                       (if (and (set? data) (>= (count data) min-size))
                         (fixed-set-matcher bindings data)
                         (async/to-chan! [])))]
    (if *seq-context*
      (matcher->seq-matcher set-matcher*)
      set-matcher*)))

(defn seq-of-matcher [registry pattern]
  (let [matcher (matcher registry pattern)]
    (fn seq-of-matcher* [bindings data]
      (if (not-empty data)
        (let [port (matcher bindings (first data))
              out-ch (distinct-chan)]
          (async/go-loop [ports []]
            (if-let [res (async/<! port)]
              (recur (conj ports (seq-of-matcher* res (rest data))))
              (if (seq ports)
                (async/pipe (async/merge ports) out-ch)
                (async/close! out-ch))))
          out-ch)
        (async/to-chan! [bindings])))))

(defn set-of-matcher [registry pattern]
  (let [matcher (let [m (binding [*seq-context* false]
                          (seq-of-matcher registry pattern))]
                  (fn [bindings data]
                    (if (set? data)
                      (m bindings (seq data))
                      (async/to-chan! []))))]
    (if *seq-context*
      (matcher->seq-matcher matcher)
      matcher)))

(defn dynamic-map-matcher [registry patterns]
  (if (seq patterns)
    (let [[[key-pattern value-pattern] & patterns] patterns
          key-matcher (matcher registry key-pattern)
          value-matcher (matcher registry value-pattern)
          continuation (dynamic-map-matcher registry patterns)]
      (fn [bindings data]
        (let [ports (for [[key value] data
                          :let [matcher
                                (sequence-matcher [key-matcher value-matcher continuation])]]
                      (matcher bindings [key value (dissoc data key)]))
              out-ch (distinct-chan)]
          (async/pipe (async/merge (vec ports)) out-ch))))
    (fn [bindings _data]
      (async/to-chan! [bindings]))))

(defn fixed-map-matcher [registry patterns continuation]
  (let [key-matchers (mapv (fn [[key pattern]]
                             [key (matcher registry pattern)])
                           patterns)
        keys (mapv first key-matchers)
        matchers (mapv second key-matchers)
        matcher (sequence-matcher matchers)]
    (fn [bindings data]
      (if (seq keys)
        (if (every? (partial contains? data) keys)
          (let [matcher (sequence-matcher [matcher continuation])]
            (matcher bindings [((apply juxt keys) data)
                               (reduce dissoc data keys)]))
          (async/to-chan! []))
        (continuation bindings data)))))

(defn map-matcher [registry & patterns]
  (let [min-size (count patterns)
        {constant-keys false
         dynamic-keys true}
        (group-by (fn [[key-pattern _]]
                    (pattern? registry key-pattern))
                  patterns)
        dynamic-map-matcher (dynamic-map-matcher registry dynamic-keys)
        fixed-map-matcher (fixed-map-matcher registry constant-keys dynamic-map-matcher)
        map-matcher* (fn [bindings data]
                       (if (and (map? data) (>= (count (seq data)) min-size))
                         (fixed-map-matcher bindings data)
                         (async/to-chan! [])))]
    (if *seq-context*
      (matcher->seq-matcher map-matcher*)
      map-matcher*)))

;; --- sequence-matchers ---

(defn cat-matcher [registry & patterns]
  (if *seq-context*
    (let [matcher (consuming-sequence-matcher (mapv (partial matcher registry) patterns))]
      (fn [bindings data]
        (if (sequential? data)
          (matcher bindings data)
          (async/to-chan! []))))
    (let [matcher (binding [*seq-context* true] (apply cat-matcher (list* registry patterns)))]
      (seq-matcher->top-level-seq-matcher matcher))))

(defn alt-matcher [registry & patterns]
  (if *seq-context*
    (let [matchers (mapv (partial matcher registry) patterns)]
      (fn [bindings data]
        (if (sequential? data)
          (async/merge (mapv #(% bindings data) matchers))
          (async/to-chan! []))))
    (let [matcher (binding [*seq-context* true] (apply alt-matcher (list* registry patterns)))]
      (seq-matcher->top-level-seq-matcher matcher))))

(defn repeat-matcher [registry {:keys [min max] :as opts} pattern]
  (if *seq-context*
    (repeat-matcher* min max 0 (matcher registry pattern))
    (let [matcher (binding [*seq-context* true] (repeat-matcher registry opts pattern))]
      (seq-matcher->top-level-seq-matcher matcher))))

(defn one-or-more-matcher [registry pattern]
  (if *seq-context*
    (repeat-matcher* 1 ##Inf 0 (matcher registry pattern))
    (let [matcher (binding [*seq-context* true] (one-or-more-matcher registry pattern))]
      (seq-matcher->top-level-seq-matcher matcher))))

(defn zero-or-more-matcher [registry pattern]
  (if *seq-context*
    (repeat-matcher* 0 ##Inf 0 (matcher registry pattern))
    (let [matcher (binding [*seq-context* true] (zero-or-more-matcher registry pattern))]
      (seq-matcher->top-level-seq-matcher matcher))))

(defn zero-or-one-matcher [registry pattern]
  (if *seq-context*
    (repeat-matcher* 0 1 0 (matcher registry pattern))
    (let [matcher (binding [*seq-context* true] (zero-or-one-matcher registry pattern))]
      (seq-matcher->top-level-seq-matcher matcher))))

;; === registry ===

(defn logic-matchers []
  {::? #'lvar-matcher
   ::! #'mvar-matcher
   ::fresh #'fresh-matcher
   ::inter #'interceptor-matcher
   ::fn #'functional-matcher})

(defn base-matchers []
  {:and #'and-matcher
   :or #'or-matcher
   :orn #'orn-matcher
   :not #'not-matcher
   :maybe #'maybe-matcher
   :enum #'enum-matcher
   :multi #'multi-matcher
   :re #'regex-matcher})

(defn collection-matchers []
  {:tuple #'tuple-matcher
   :set #'set-matcher
   :set-of #'set-of-matcher
   :map #'map-matcher
   ;; :map-of #'map-of-matcher
   })

(defn comparator-matcher [comparator]
  (fn [_ value]
    (letfn [(comparator-matcher* [bindings data]
              (async/to-chan! (when (try
                                      (comparator data value)
                                      (catch #?(:clj Throwable :cljs js/Error) _))
                                [bindings])))]
      (if *seq-context*
        (matcher->seq-matcher comparator-matcher*)
        comparator-matcher*))))

(defn comparator-matchers []
  (reduce
   (fn [acc comparator]
     (let [matcher (comparator-matcher comparator)
           name (-> comparator meta :name)]
       (assoc acc
              name matcher
              (keyword name) matcher
              @comparator matcher)))
   {}
   [#'> #'>= #'< #'<= #'= #'== #'not=]))

(defn predicate-matcher [predicate]
  (fn [_]
    (letfn [(predicate-matcher* [bindings data]
              (async/to-chan! (when (predicate data) [bindings])))]
      (if *seq-context*
        (matcher->seq-matcher predicate-matcher*)
        predicate-matcher*))))

(defn predicate-matchers []
  (reduce
   (fn [acc predicate]
     (let [matcher (predicate-matcher predicate)
           name (-> predicate meta :name)]
       (assoc acc
              name matcher
              (keyword name) matcher
              @predicate matcher)))
   {}
   [#'any? #'some? #'number? #'integer? #'int? #'pos-int? #'neg-int? #'nat-int? #'pos? #'neg? #'float? #'double?
    #'boolean? #'string? #'ident? #'simple-ident? #'qualified-ident? #'keyword? #'simple-keyword?
    #'qualified-keyword? #'symbol? #'simple-symbol? #'qualified-symbol? #'uuid? #'uri? #?(:clj #'decimal?)
    #'inst? #'seqable? #'indexed? #'map? #'vector? #'list? #'seq? #'char? #'set? #'nil? #'false? #'true?
    #'zero? #?(:clj #'rational?) #'coll? #'empty? #'associative? #'sequential? #?(:clj #'ratio?) #?(:clj #'bytes?)]))

(defn type-matchers []
  (reduce
   (fn [acc [key predicate]]
     (assoc acc key (predicate-matcher predicate)))
   {}
   {:any #'any?
    :nil #'nil?
    :string #'string?
    :int #'int?
    :double #'double?
    :boolean #'boolean?
    :keyword #'keyword?
    :symbol #'symbol?
    :qualified-keyword #'qualified-keyword?
    :qualified-symbol #'qualified-symbol?
    :uuid #'uuid?}))

(defn sequence-matchers []
  {:cat #'cat-matcher
   :alt #'alt-matcher
   :repeat #'repeat-matcher
   :+ #'one-or-more-matcher
   :* #'zero-or-more-matcher
   :? #'zero-or-one-matcher})

(defn default-matchers-registry []
  (atom (merge (logic-matchers)
               (base-matchers)
               (collection-matchers)
               (comparator-matchers)
               (predicate-matchers)
               (type-matchers)
               (sequence-matchers))))

;; === api ===

(defn matcher
  ([pattern]
   (matcher (default-matchers-registry) pattern))
  ([registry pattern]
   (let [token (if (vector? pattern) (first pattern) pattern)
         args (if (vector? pattern) (rest pattern) [])]
     (if-let [builder (get @registry token)]
       (apply builder (list* registry args))
       (throw (ex-info "Undefined instruction token" {:token token}))))))

;; ===

(comment

  (def logs (atom []))

  @logs

  (add-tap (fn [entry]
             (swap! logs conj entry)))

  (defn <!! [port]
    (async/<!! (async/go-loop [acc []]
                 (if-let [res (async/<! port)]
                   (recur (conj acc res))
                   acc))))

  (<!! ((matcher [:orn {:key :branch} [:first [::? :x]] [:second [::? :y]]]) {} 42))

  (<!! ((matcher [:tuple [:= 1] [:= 2] [:= 3]]) {} [1 2 3]))

  (<!! ((matcher [:set 1 [::? :x] 3]) {} #{1 2 3 4 5}))

  (distinct (<!! ((matcher [:map
                            [[::? :k1] [:= 1]]
                            [[::? :k2] [:= 1]]])
                  {} {:x 1 :y 1 :z 1})))

  (let [m (matcher [:cat [:and [:cat [:+ int?] [:+ string?]] [::? :x]]])]
    (time
     (dotimes [_ 1000]
       (<!! (m {} [1 1 1 "foo"]))))
    (async/<!! (m {} [1 1 1 "foo"])))

  (<!! ((matcher [:cat [:tuple int? int?] [:+ [::! :x]]]) {} [1 2 3 4 5]))

  (<!! ((matcher [:cat [:* [:and [:= "a"] [::! :x]]] [:* [:= "a"]] [:* [:= "a"]] [:= "b"]]) {} (concat (repeat 12 "a") ["b"])))

  (<!! ((matcher [:cat [:and [:cat [:+ int?] [:+ string?]] [::? :x]]])
        {} [1 1 "foo" 1 1]))

  (<!! ((matcher [:cat
                  [:and [:+ [:= "a"]] [::? :a]]
                  [:and [:+ [:= "a"]] [::? :b]]])
        {} ["a" "a" "a" "a" "a" "a" "a"]))

  (def poker-hand-matcher
    (letfn [(plus-match-fn
              ([key] (plus-match-fn key 0))
              ([key k]
               (fn [bindings data]
                 (if-let [binding (and (contains? bindings key)
                                       (get bindings key))]
                   (when (= data (+ binding k))
                     [bindings])
                   [(assoc bindings key (- data k))]))))
            (high-card [{:keys [rank] :as bindings} data]
              [(if (or (nil? rank) (>= data rank))
                 (assoc bindings :rank data)
                 bindings)])]
      (matcher
       [:orn {:key :hand}
        [:strait-flush [:set
                        [:tuple [::? :suit] [::fn (plus-match-fn :rank)]]
                        [:tuple [::? :suit] [::fn (plus-match-fn :rank 1)]]
                        [:tuple [::? :suit] [::fn (plus-match-fn :rank 2)]]
                        [:tuple [::? :suit] [::fn (plus-match-fn :rank 3)]]
                        [:tuple [::? :suit] [::fn (plus-match-fn :rank 4)]]]]
        [:strait [:set
                  [:tuple any? [::fn (plus-match-fn :rank)]]
                  [:tuple any? [::fn (plus-match-fn :rank 1)]]
                  [:tuple any? [::fn (plus-match-fn :rank 2)]]
                  [:tuple any? [::fn (plus-match-fn :rank 3)]]
                  [:tuple any? [::fn (plus-match-fn :rank 4)]]]]
        [:royal-flush [:set
                       [:tuple [::? :suit] [:= 10]]
                       [:tuple [::? :suit] [:= 11]]
                       [:tuple [::? :suit] [:= 12]]
                       [:tuple [::? :suit] [:= 13]]
                       [:tuple [::? :suit] [:= 14]]]]
        [:four-of-kind [:set
                        any?
                        [:tuple any? [::? :rank]]
                        [:tuple any? [::? :rank]]
                        [:tuple any? [::? :rank]]
                        [:tuple any? [::? :rank]]]]
        [:three-of-kind [:set
                         any?
                         any?
                         [:tuple any? [::? :rank]]
                         [:tuple any? [::? :rank]]
                         [:tuple any? [::? :rank]]]]
        [:full-house [:set
                      [:tuple any? [::? :full]]
                      [:tuple any? [::? :full]]
                      [:tuple any? [::? :full]]
                      [:tuple any? [::? :house]]
                      [:tuple any? [::? :house]]]]
        [:pair [:set
                any?
                any?
                any?
                [:tuple any? [::? :rank]]
                [:tuple any? [::? :rank]]]]
        [:two-pairs [:set
                     any?
                     [:tuple any? [::? :rank-1]]
                     [:tuple any? [::? :rank-1]]
                     [:tuple any? [::? :rank-2]]
                     [:tuple any? [::? :rank-2]]]]
        [:flush [:set-of [:tuple [::? :suit] any?]]]
        [:highest-card [:set-of [:tuple any? [::fn high-card]]]]])))

  (time
   (dotimes [_ 100]
     (<!! (poker-hand-matcher {}
                              #{[:♠ 10] [:♠ 11] [:♠ 12] [:♠ 13] [:♠ 14]}
                              ))))

  (async/<!! ((matcher [:set
                        any?
                        [:tuple any? [::? :rank]]
                        [:tuple any? [::? :rank]]
                        [:tuple any? [::? :rank]]
                        [:tuple any? [::? :rank]]])
              {} #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 5]}))

  (async/<!! ((matcher [:set any? [:tuple any? [::? :rank]]]) {} #{[:♥ 5] 1}))

  )
