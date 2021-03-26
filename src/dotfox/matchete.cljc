(ns dotfox.matchete
  (:require [clojure.core.async :as async]
            [sci.core :as sci]))

(defn- distinct-channel [form]
  (case (count form)
    1 `(async/chan 1024 (distinct))
    2 (case (second form)
        1 form
        (concat form '((distinct))))
    form))

(defmacro with-chan [[ch form] & body]
  `(let [~ch ~(distinct-channel form)]
     ~@body
     ~ch))

(declare matcher)

(defn lvar-pattern [_registry binding]
  (fn f
    ([data] (f {} data))
    ([bindings data]
     (async/go
       (cond
         (and (contains? bindings binding)
              (= data (get bindings binding)))
         bindings

         (not (contains? bindings binding))
         (assoc bindings binding data))))))

(defn mvar-pattern [_registry binding]
  (fn f
    ([data] (f {} data))
    ([bindings data]
     (async/go
       (update bindings binding (fnil conj []) data)))))

(defn- and-matcher [matchers]
  (if (seq matchers)
    (let [[matcher & matchers] matchers
          next-matcher (and-matcher matchers)]
      (fn [bindings data]
        (with-chan [ch (async/chan)]
          (async/go-loop [port-1 (matcher bindings data)]
            (if-let [res (async/<! port-1)]
              (do (loop [port-2 (next-matcher res data)]
                    (when-let [res (async/<! port-2)]
                      (async/>! ch res)
                      (recur port-2)))
                  (recur port-1))
              (async/close! ch))))))
    (fn [bindings _data]
      (async/go bindings))))

(defn and-pattern [registry & patterns]
  (let [matchers (doall (map (partial matcher registry) patterns))
        chain (and-matcher matchers)]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (with-chan [ch (async/chan)]
         (async/go-loop [port (chain bindings data)]
           (if-let [res (async/<! port)]
             (do (async/>! ch res)
                 (recur port))
             (async/close! ch))))))))

(defn or-pattern [registry & patterns]
  (let [matchers (doall (map (partial matcher registry) patterns))]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (with-chan [ch (async/chan)]
         (async/go-loop [ports (set (map #(% bindings data) matchers))]
           (if-let [ports' (seq ports)]
             (let [[res port] (async/alts! ports')]
               (if (some? res)
                 (do (async/>! ch res)
                     (recur ports))
                 (recur (disj ports port))))
             (async/close! ch))))))))

(defn orn-pattern [registry {:keys [key]} & named-forms]
  (let [matchers (into {} (map (fn [[name pattern]]
                                 [name (matcher registry pattern)]))
                       named-forms)]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (with-chan [ch (async/chan)]
         (let [binding-ports (into {} (map (fn [[name matcher]]
                                             [(matcher bindings data) name]))
                                   matchers)]
           (async/go-loop [ports (set (keys binding-ports))]
             (if-let [ports' (seq ports)]
               (let [[res port] (async/alts! ports')]
                 (if res
                   (do (async/>! ch (assoc res key (get binding-ports port)))
                       (recur ports))
                   (recur (disj ports port))))
               (async/close! ch)))))))))

(defn not-pattern [registry pattern]
  (let [matcher (matcher registry pattern)]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (let [port (matcher bindings data)]
         (async/go
           (when-not (async/<! port)
             bindings)))))))

(defn- sequential-matcher [[matcher & matchers]]
  (if matcher
    (let [post-fn (sequential-matcher matchers)]
      (fn [bindings data]
        (with-chan [ch (async/chan)]
          (if (empty? data)
            (async/close! ch)
            (let [ch' (matcher bindings (first data))]
              (async/go-loop []
                (if-let [res (async/<! ch')]
                  (let [ch'' (post-fn res (rest data))]
                    (loop []
                      (when-let [res (async/<! ch'')]
                        (async/>! ch res)
                        (recur)))
                    (recur))
                  (async/close! ch))))))))
    (fn [bindings _data]
      (async/go bindings))))

(defn tuple-pattern [registry & patterns]
  (let [matchers (doall (map (partial matcher registry) patterns))
        matcher (sequential-matcher matchers)]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (if (= (count matchers)
              (count data))
         (with-chan [ch (async/chan)]
           (let [port (matcher bindings data)]
             (async/go-loop []
               (if-let [res (async/<! port)]
                 (do (async/>! ch res)
                     (recur))
                 (async/close! ch)))))
         (async/go nil))))))

(defn- dynamic-key? [registry [k-pattern _]]
  (and (vector? k-pattern) (contains? @registry (first k-pattern))))

(defn- fixed-map-pattern [registry kv-patterns continuation]
  (let [matchers (into {} kv-patterns)
        keys (keys matchers)
        extractor (when (not-empty keys) (apply juxt keys))
        matcher (matcher registry (vec (list* :tuple (mapv matchers keys))))]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (cond
         (empty? keys)
         (continuation bindings data)

         (every? #(contains? data %) keys)
         (if-let [data' (not-empty (extractor data))]
           (let [data'' (reduce dissoc data keys)]
             (with-chan [ch (async/chan)]
               (async/go-loop [port-1 (matcher bindings data')]
                 (if-let [res (async/<! port-1)]
                   (do (loop [port-2 (continuation res data'')]
                         (when-let [res (async/<! port-2)]
                           (async/>! ch res)
                           (recur port-2)))
                       (recur port-1))
                   (async/close! ch)))))
           (continuation bindings data))

         :else
         (async/go nil))))))

(defn- dynamic-map-pattern [registry kv-patterns]
  (if-let [[[k-pattern v-pattern] & kv-patterns] (seq kv-patterns)]
    (let [k-matcher (matcher registry k-pattern)
          v-matcher (matcher registry v-pattern)
          chain (dynamic-map-pattern registry kv-patterns)]
      (fn [bindings data]
        (if-let [keys (keys data)]
          (with-chan [ch (async/chan)]
            (let [k-chain (fn [k]
                            (with-chan [ch' (async/chan)]
                              (let [k-port (k-matcher bindings k)]
                                (async/go-loop []
                                  (if-let [res (async/<! k-port)]
                                    (let [v-port (v-matcher res (get data k))]
                                      (loop []
                                        (when-let [res (async/<! v-port)]
                                          (let [port (chain res (dissoc data k))]
                                            (loop []
                                              (when-let [res (async/<! port)]
                                                (async/>! ch' res)
                                                (recur))))
                                          (recur)))
                                      (recur))
                                    (async/close! ch'))))))]
              (async/go-loop [ports (into #{} (map k-chain) keys)]
                (if-let [ports' (seq ports)]
                  (let [[res port] (async/alts! ports')]
                    (if res
                      (do (async/>! ch res)
                          (recur ports))
                      (recur (disj ports port))))
                  (async/close! ch)))))
          (async/go nil))))
    (fn [bindings _data]
      (async/go bindings))))

(defn map-pattern [registry & kv-patterns]
  (let [{fixed-keys false dynamic-keys true}
        (group-by (partial dynamic-key? registry) kv-patterns)
        dynamic-map-matcher (dynamic-map-pattern registry dynamic-keys)
        fixed-map-matcher (fixed-map-pattern registry fixed-keys dynamic-map-matcher)]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (if (map? data)
         (with-chan [ch (async/chan)]
           (async/go-loop [port (fixed-map-matcher bindings data)]
             (if-let [res (async/<! port)]
               (do (async/>! ch res)
                   (recur port))
               (async/close! ch))))
         (async/go nil))))))

(defn- sequence-of-matcher [matcher]
  (fn [bindings data]
    (if (empty? data)
      (async/go bindings)
      (let [continuation (sequence-of-matcher matcher)]
        (with-chan [ch (async/chan)]
          (async/go-loop [port-1 (matcher bindings (first data))]
            (if-let [res (async/<! port-1)]
              (do (loop [port-2 (continuation res (rest data))]
                    (when-let [res (async/<! port-2)]
                      (async/>! ch res)
                      (recur port-2)))
                  (recur port-1))
              (async/close! ch))))))))

(defn map-of-pattern [registry k-pattern v-pattern]
  (let [matcher (sequence-of-matcher (matcher registry [:tuple k-pattern v-pattern]))]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (if (map? data)
         (matcher bindings data)
         (async/go nil))))))

(defn vector-pattern [registry pattern]
  (let [matcher (sequence-of-matcher (matcher registry pattern))]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (if (vector? data)
         (matcher bindings data)
         (async/go nil))))))

(defn sequential-pattern [registry pattern]
  (let [matcher (sequence-of-matcher (matcher registry pattern))]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (if (sequential? data)
         (matcher bindings data)
         (async/go nil))))))

(defn- set-matcher [matchers]
  (if-let [[matcher & matchers] (seq matchers)]
    (let [continuation (set-matcher matchers)]
      (fn [bindings data]
        (with-chan [ch (async/chan)]
          (let [ports (into #{}
                            (map (fn [el]
                                   (with-chan [ch (async/chan)]
                                     (async/go-loop [port (matcher bindings el)]
                                       (if-let [res (async/<! port)]
                                         (do
                                           (loop [port (continuation res (disj data el))]
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
    (fn [bindings _data]
      (async/go bindings))))

(defn set-pattern [registry & patterns]
  (let [matcher (set-matcher (doall (map (partial matcher registry) patterns)))]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (if (and (set? data)
                (>= (count data) (count patterns)))
         (matcher bindings data)
         (async/go nil))))))

(defn set-of-pattern [registry pattern]
  (let [matcher (sequential-matcher (matcher registry pattern))]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (if (set? data)
         (matcher bindings (seq data))
         (async/go nil))))))

(defn enum-pattern [registry & values]
  (matcher registry (vec (list* :or (map #(vector := %) values)))))

(defn- compare-matcher [operator value]
  (fn f
    ([data] (f {} data))
    ([bindings data]
     (async/go
       (when (operator data value)
         bindings)))))

(defn maybe-pattern [registry pattern]
  (let [matcher (matcher registry pattern)]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (if (nil? data)
         (async/go bindings)
         (matcher bindings data))))))

(defn multi-pattern [registry {:keys [dispatch]} & patterns]
  (let [matchers (into {}
                       (map (fn [[dispatch-value pattern]]
                              [dispatch-value (matcher registry pattern)]))
                       patterns)]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (let [dispatch-value (dispatch data)]
         (if-let [matcher (matchers dispatch-value (:default matchers))]
           (matcher bindings data)
           (async/go nil)))))))

(defn- fn-builder [form]
  ;; TODO check which sci options might be usefull
  (sci/eval-form (sci/init {}) form))

(defn dynamic-predicate-pattern [_registry form]
  (let [predicate (fn-builder form)]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (async/go (when (predicate data) bindings))))))

(defn regex-pattern [_registry regex]
  (let [regex (cond
                (string? regex)
                (re-pattern regex)

                (instance? #?(:clj java.util.regex.Pattern
                              :cljs js/RegExp)
                           regex)
                regex

                :else
                (throw (ex-info "Invalid regex form" {:form regex})))]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (async/go (when (and (string? data) (re-matches regex data))
                   bindings))))))

(defn ref-pattern [registry pattern-id]
  (fn f
    ([data] (f {} data))
    ([bindings data]
     (if-let [matcher (get-in @registry [::dynamic pattern-id])]
       (matcher bindings data)
       ;; TODO this should be an unknown pattern error
       (async/go nil)))))

(defn schema-pattern [registry named-patterns pattern]
  (let [named-patterns (into {}
                             (map (fn [[name pattern]]
                                    [name (matcher registry pattern)]))
                             named-patterns)
        _ (swap! registry update ::dynamic merge named-patterns)
        matcher (matcher registry pattern)]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (matcher bindings data)))))

(defn guard-pattern [_registry function]
  (let [function (if (and (vector? function) (= :fn (first function)))
                   (fn-builder (second function))
                   function)]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (let [results (function bindings data)]
         (cond
           (map? results)
           (async/go results)

           (and (sequential? results)
                (every? map? results))
           (with-chan [ch (async/chan)]
             (async/go-loop [results results]
               (if-let [[result & results] results]
                 (do (async/>! ch result)
                     (recur results))
                 (async/close! ch))))

           :else
           (async/go nil)))))))

(defn- repeat-matcher [min max matcher]
  (letfn [(step [bindings data current]
            (cond
              (> current max)
              (async/go nil)

              (empty? data)
              (async/go (when (>= current min) bindings))

              :else
              (with-chan [ch (async/chan)]
                (async/go-loop [port-1 (matcher bindings (first data))]
                  (if-let [res (async/<! port-1)]
                    (do (loop [port-2 (step res (rest data) (inc current))]
                          (when-let [res (async/<! port-2)]
                            (async/>! ch res)
                            (recur port-2)))
                        (recur port-1))
                    (async/close! ch))))))]
    (fn [bindings data]
      (if (sequential? data)
        (step bindings data 0)
        (async/go nil)))))

(defn repeat-pattern
  ([registry pattern]
   (repeat-pattern registry {} pattern))
  ([registry {:keys [min max]
              :or {min 0
                   max ##Inf}} pattern]
   (let [matcher (repeat-matcher min max (matcher registry pattern))]
     (fn f
       ([data] (f {} data))
       ([bindings data]
        (matcher bindings data))))))

(defn cat-pattern [registry & patterns]
  (let [matcher (sequential-matcher (doall (map (partial matcher registry) patterns)))
        size (count patterns)]
    (fn f
      ([data] (f {} data))
      ([bindings data]
       (if (and (sequential? data)
                (>= (count data) size))
         (matcher bindings data)
         (async/go nil))))))

(defn alt-pattern [registry & patterns]
  (let [pattern [:repeat (vec (list* :or patterns))]]
    (matcher registry pattern)))

(defn base-patterns []
  {::? #'lvar-pattern
   ::! #'mvar-pattern
   ::guard #'guard-pattern
   :and #'and-pattern
   :or #'or-pattern
   :orn #'orn-pattern
   :not #'not-pattern
   :map #'map-pattern
   :map-of #'map-of-pattern
   :vector #'vector-pattern
   :sequential #'sequential-pattern
   :set #'set-pattern
   :set-of #'set-of-pattern
   :enum #'enum-pattern
   :maybe #'maybe-pattern
   :tuple #'tuple-pattern
   :multi #'multi-pattern
   ;; :re #'regex-pattern
   :fn #'dynamic-predicate-pattern
   :ref #'ref-pattern
   :schema #'schema-pattern})

(defn comparator-patterns []
  (into {}
        (map (fn [[k operator]]
               [k (fn [_ value]
                    (compare-matcher operator value))]))
        {:> >, :>= >=, :< <, :<= <=, := =, :not= not=}))

(defn predicate-patterns []
  (reduce
   (fn [acc predicate]
     (let [builder (fn [_registry]
                     (fn f
                       ([data] (f {} data))
                       ([bindings data]
                        (async/go (when (predicate data) bindings)))))
           name (-> predicate meta :name)
           keyword (keyword name)]
       (assoc acc
              name builder
              keyword builder
              @predicate builder)))
   {}
   [#'any? #'some? #'number? #'integer? #'int? #'pos-int? #'neg-int? #'nat-int? #'pos? #'neg? #'float? #'double?
    #'boolean? #'string? #'ident? #'simple-ident? #'qualified-ident? #'keyword? #'simple-keyword?
    #'qualified-keyword? #'symbol? #'simple-symbol? #'qualified-symbol? #'uuid? #'uri? #?(:clj #'decimal?)
    #'inst? #'seqable? #'indexed? #'map? #'vector? #'list? #'seq? #'char? #'set? #'nil? #'false? #'true?
    #'zero? #?(:clj #'rational?) #'coll? #'empty? #'associative? #'sequential? #?(:clj #'ratio?) #?(:clj #'bytes?)]))

(defn type-patterns []
  (reduce
   (fn [acc [key predicate]]
     (let [builder (fn [_registry]
                     (fn f
                       ([data] (f {} data))
                       ([bindings data]
                        (async/go (when (predicate data) bindings)))))]
       (assoc acc key builder)))
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

(defn sequence-patterns []
  {:+ #(repeat-pattern %1 {:min 1} %2)
   :* #(repeat-pattern %1 {} %2)
   :? #(repeat-pattern %1 {:min 0 :max 1} %2)
   :repeat #'repeat-pattern
   :cat #'cat-pattern
   :alt #'alt-pattern})

(defn default-patterns-registry []
  (atom (merge (base-patterns)
               (comparator-patterns)
               (predicate-patterns)
               (type-patterns)
               (sequence-patterns))))

(defn matcher
  ([pattern]
   (matcher (default-patterns-registry) pattern))
  ([registry pattern]
   (let [token (if (vector? pattern) (first pattern) pattern)
         args (if (vector? pattern) (rest pattern) [])]
     (if-let [builder (get @registry token)]
       (apply builder (list* registry args))
       (throw (ex-info "Undefined instruction token" {:token token}))))))

(comment

  (defn <all [port]
    (async/<!! (async/go-loop [acc []]
                 (if-let [res (async/<! port)]
                   (recur (conj acc res))
                   acc))))

  (<all ((matcher [::? :x]) 42))

  (<all ((matcher [::! :x]) 42))

  (<all ((matcher [:and [::? :x] [::! :y]]) 42))

  (<all ((matcher [:or [::? :x] [::? :y]]) 42))

  (<all ((matcher [:orn {:key :branch} [:1 [::? :x]] [:2 [::? :x]]]) 42))

  (<all ((matcher [:not [:and [::! :x] [::? :x]]]) 42))

  (<all ((matcher [:tuple [::? :x] [::? :y]]) [1 2]))

  (let [m (matcher [:map
                    [:x [::? :x]]
                    [:y [::? :y]]
                    [[::? :extra-key] [::? :extra-value]]
                    [[::? :extra-key-2] [::? :extra-value-2]]])]
    (time
     (dotimes [_ 10000]
       (<all (m {:x 1 :y 2 :l 3 :n 4 :m 5}))))
    (<all (m {:x 1 :y 2 :l 3 :n 4 :m 5})))

  (<all ((matcher [:map-of [::! :key] [::! :value]]) {:x 1 :y 2}))

  (<all ((matcher [:set [::? :x] [::? :y] [::? :z]]) #{1 2}))

  (<all ((matcher [:enum 1 2 3]) 2))

  (<all ((matcher [:not= 42]) 41))

  (<all ((matcher [:vector :int?]) [1 2 3 ]))

  (<all ((matcher [:schema {:step [:map
                                   [[::! :path] [:or
                                                 [:ref :step]
                                                 [:and
                                                  [:not :map?]
                                                  [::? :value]]]]]}
                   [:ref :step]])
         {:x 1
          :y {:z 2}}))

  (<all ((matcher [:map [[::! :path] [::? :value]]]) {:x 1 :y 2}))

  (<all ((matcher [:multi {:dispatch :type}
                   [:sized [:map
                            [:type keyword?]
                            [:size int?]]]
                   [:human [:map
                            [:type keyword?]
                            [:name string?]
                            [:address [:map
                                       [:country keyword?]]]]]])
         {:type :sized, :size 10}))

  (<all ((matcher [:repeat {:min 2 :max 5} [:and int? [::! :el]]])
         [1 2 3 4 5 6]))

  (<all ((matcher [:? int?]) [1]))

  (<all ((matcher [:+ int?]) [1 2]))

  (<all ((matcher [:* int?]) []))

  )
