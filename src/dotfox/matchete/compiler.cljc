(ns dotfox.matchete.compiler
  (:refer-clojure :exclude [compile])
  (:require [dotfox.matchete.base :as base]
            [dotfox.matchete.collection :as collection]
            [dotfox.matchete.seqexp :as seqexp]))

(declare base-compile)

(defn epsilon [_ _]
  (base/epsilon))

(defn lvar [_ [_ binding]]
  (base/lvar-matcher binding))

(defn mvar [_ [_ binding]]
  (base/mvar-matcher binding))

(defn wrap [registry [_ {:keys [enter leave]} pattern]]
  (let [matcher (base-compile registry pattern)]
    (base/wrap-matcher {:entry enter :exit leave} matcher)))

(defn and* [registry [_ & patterns]]
  (let [matchers (mapv (partial base-compile registry) patterns)]
    (base/and-matcher matchers)))

(defn or* [registry [_ & patterns]]
  (base/or-matcher (map (partial base-compile registry) patterns)))

(defn orn [registry [_ {:keys [branch-key]} & patterns]]
  (let [matchers (into {}
                       (map (fn [[branch-value pattern]]
                              [branch-value (base-compile registry pattern)]))
                       patterns)]
    (base/orn-matcher branch-key matchers)))

(defn not* [registry [_ pattern]]
  (base/not-matcher (base-compile registry pattern)))

(defn maybe [registry [_ pattern]]
  (base/maybe-matcher (base-compile registry pattern)))

(defn re [_ [_ regex]]
  (base/regex-matcher
   (cond
     (string? regex)
     (re-pattern regex)

     (instance? #?(:clj java.util.regex.Pattern :cljs js/RegExp) regex)
     regex

     :else
     (throw (ex-info "Expect pattern or string" {})))))

(defn multi [registry [_ {:keys [dispatch-fn]} & patterns]]
  (let [matchers (into {}
                       (map (fn [[dispatch-value pattern]]
                              [dispatch-value (base-compile registry pattern)]))
                       patterns)]
    (base/multi-matcher dispatch-fn matchers)))

(defn pred [registry [_ pred]]
  (base/pred-matcher (base-compile registry pred)))

(defn function [_ [_ f]]
  (base/function-matcher f))

(defn ref* [{::keys [custom]} [_ id]]
  (fn [bindings data]
    ((get @custom id) bindings data)))

(defn base-registry []
  {:eps epsilon
   :lvar lvar
   :mvar mvar
   :wrap wrap
   :and and*
   :or or*
   :orn orn
   :not not*
   :maybe maybe
   :re re
   :multi multi
   :pred pred
   :fn function
   :ref ref*})

(defn pred-registry []
  (reduce
   (fn [acc pred]
     (let [name (-> pred meta :name)
           key (keyword name)
           pred @pred
           matcher (base/pred-matcher pred)
           compiler (fn [_ _] matcher)]
       (assoc acc
              name compiler
              key compiler
              pred compiler)))
   {}
   [#'any? #'some? #'number? #'integer? #'int? #'pos-int? #'neg-int? #'nat-int? #'pos? #'neg? #'float? #'double?
    #'boolean? #'string? #'ident? #'simple-ident? #'qualified-ident? #'keyword? #'simple-keyword?
    #'qualified-keyword? #'symbol? #'simple-symbol? #'qualified-symbol? #'uuid? #'uri? #?(:clj #'decimal?)
    #'inst? #'seqable? #'indexed? #'map? #'vector? #'list? #'seq? #'char? #'set? #'nil? #'false? #'true?
    #'zero? #?(:clj #'rational?) #'coll? #'empty? #'associative? #'sequential? #?(:clj #'ratio?) #?(:clj #'bytes?)]))

(defn compare-registry []
  (reduce
   (fn [acc comparator]
     (let [name (-> comparator meta :name)
           key (keyword name)
           comparator @comparator
           compiler (fn [_ [_ value]]
                      (base/pred-matcher #(comparator % value)))]
       (assoc acc
              name compiler
              key compiler
              comparator compiler)))
   {}
   [#'> #'>= #'< #'<= #'= #'== #'not=]))

(defn types-registry []
  (reduce
   (fn [acc pred]
     (let [name (symbol (apply str (butlast (name (-> pred meta :name)))))
           key (keyword name)
           pred @pred
           matcher (base/pred-matcher pred)
           compiler (fn [_ _] matcher)]
       (assoc acc
              name compiler
              key compiler)))
   {}
   [#'symbol? #'qualified-symbol?
    #'keyword? #'qualified-keyword?
    #'nil? #'string? #'double? #'int? #'boolean?
    #'uuid? #'any?]))

(defn sequence* [registry [_ & patterns]]
  (collection/sequence-matcher (map (partial base-compile registry) patterns)))

(defn sequence-of [registry [_ pattern]]
  (collection/sequence-of-matcher (base-compile registry pattern)))

(defn tuple [registry [_ & patterns]]
  (collection/tuple-matcher (map (partial base-compile registry) patterns)))

(defn set* [registry [_ & patterns]]
  (collection/set-matcher (map (partial base-compile registry) patterns)))

(defn set-of [registry [_ pattern]]
  (collection/set-of-matcher (base-compile registry pattern)))

(defn map* [registry [_ & patterns]]
  (collection/map-matcher
   (map
    (fn [[key-pattern value-pattern]]
      (base-compile registry [:tuple key-pattern value-pattern]))
    patterns)))

(defn map-of [registry [_ key-pattern value-pattern]]
  (collection/map-of-matcher (base-compile registry [:tuple key-pattern value-pattern])))

(defn search [registry [_ & patterns]]
  (apply collection/search-matcher (map (partial base-compile registry) patterns)))

(defn collection-registry []
  {:list sequence*
   :list-of sequence-of
   :vector sequence*
   :vector-of sequence-of
   :tuple tuple
   :set set*
   :set-of set-of
   :map map*
   :map-of map-of
   :search search})

(declare seqexp-compile)

(defn seqexp-eps [_ _]
  (seqexp/epsilon))

(defn seqexp-and [registry [_ & patterns]]
  (seqexp/and-matcher (map (partial seqexp-compile registry) patterns)))

(defn seqexp-cat [registry [_ & patterns]]
  (seqexp/cat-matcher (map (partial seqexp-compile registry) patterns)))

(defn seqexp-alt [registry [_ & patterns]]
  (base/or-matcher (map (partial seqexp-compile registry) patterns)))

(defn seqexp-altn [registry [_ {:keys [branch-key]} & patterns]]
  (let [matchers (into {}
                       (map (fn [[branch-value pattern]]
                              [branch-value (seqexp-compile registry pattern)]))
                       patterns)]
    (base/orn-matcher branch-key matchers)))

(defn seqexp-repeat [registry [_ {:keys [min max]} pattern]]
  (seqexp/repeat-matcher min max (seqexp-compile registry pattern)))

(defn seqexp-registry []
  (reduce
   (fn [acc token]
     (let [compiler (get-in acc [::seqexp/registry token])]
       (assoc acc token (fn [registry pattern]
                          (let [matcher (compiler registry pattern)
                                size-key (keyword seqexp/this-ns (name (gensym)))]
                            (base/wrap-matcher
                             {:entry (fn [bindings data]
                                       [(assoc bindings size-key (count data)) data])
                              :exit (fn [{::seqexp/keys [consumed] :as bindings}]
                                      (let [original-size (get bindings size-key)]
                                        (when (= original-size consumed)
                                          [(dissoc bindings ::seqexp/consumed size-key)])))}
                             matcher))))))
   {::seqexp/registry
    (reduce-kv
     (fn [acc token [min max]]
       (let [compiler (fn [registry [_ pattern]]
                        (seqexp/repeat-matcher min max (seqexp-compile registry pattern)))]
         (assoc acc token compiler)))
     {:eps seqexp-eps
      :and seqexp-and
      :cat seqexp-cat
      :alt seqexp-alt
      :altn seqexp-altn
      :repeat seqexp-repeat}
     {:* [0 ##Inf]
      :+ [1 ##Inf]
      :? [0 1]})}
   [:cat :alt :altn :repeat :* :+ :?]))

(defn default-registry []
  (merge (base-registry)
         (pred-registry)
         (compare-registry)
         (types-registry)
         (collection-registry)
         (seqexp-registry)
         {::custom (atom {})}))

(defn normalize [registry pattern]
  (cond
    (and (vector? pattern)
         (contains? registry (first pattern)))
    pattern

    (contains? registry pattern)
    [pattern]

    (contains? @(::custom registry) pattern)
    [:ref pattern]

    (map? pattern)
    (vec (list* :map (seq pattern)))

    (vector? pattern)
    (vec (list* :tuple (seq pattern)))

    (set? pattern)
    (vec (list* :set (seq pattern)))

    (sequential? pattern)
    (vec (list* :list (seq pattern)))

    :else
    [:= pattern]))

(defn ->seqexp-compiler [compiler]
  (fn [registry pattern]
    (seqexp/->seqexp-matcher (compiler registry pattern))))

(defn seqexp-compile [registry pattern]
  (let [[token & _ :as pattern'] (normalize registry pattern)
        compiler (get-in registry [::seqexp/registry token]
                         (->seqexp-compiler (get registry token)))]
    (compiler registry pattern')))

(defn base-compile [registry pattern]
  (let [[token & _ :as pattern'] (normalize registry pattern)]
    ((get registry token) registry pattern')))

(defn compile
  ([pattern] (compile nil pattern))
  ([registry pattern]
   (let [registry (reduce-kv
                   (fn [{::keys [custom] :as acc} k v]
                     (swap! custom assoc k (base-compile acc v))
                     acc)
                   (default-registry)
                   registry)]
     (base-compile registry pattern))))
