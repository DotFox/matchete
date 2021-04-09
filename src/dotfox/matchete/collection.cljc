(ns dotfox.matchete.collection
  (:require [dotfox.matchete.base :refer [cross-join epsilon] :as base]))

(defn sequence-matcher [matchers]
  (if (seq matchers)
    (let [[matcher & matchers] matchers
          continuation (sequence-matcher matchers)]
      (fn [bindings data]
        (when (seq data)
          (cross-join (for [res (matcher bindings (first data))]
                        (continuation res (rest data)))))))
    (fn [bindings _data]
      (list bindings))))

(defn sequence-of-matcher [matcher]
  (fn sequence-of-matcher* [bindings data]
    (when (sequential? data)
      (if (seq data)
        (cross-join (for [res (matcher bindings (first data))]
                      (sequence-of-matcher* res (rest data))))
        (list bindings)))))

(defn tuple-matcher [matchers]
  (let [size (count matchers)
        matcher (sequence-matcher matchers)]
    (fn [bindings data]
      (when (and (sequential? data)
                 (= size (count data)))
        (matcher bindings data)))))

(defn set-matcher* [matchers]
  (if (seq matchers)
    (let [[matcher & matchers] matchers
          continuation (set-matcher* matchers)]
      (fn [bindings data]
        (cross-join (for [el data
                          res (matcher bindings el)]
                      (continuation res (disj data el))))))
    (epsilon)))

(defn set-matcher [matchers]
  (let [matcher (set-matcher* matchers)
        min-size (count matchers)]
    (fn [bindings data]
      (when (and (set? data) (>= (count data) min-size))
        (matcher bindings data)))))

(defn set-of-matcher [matcher]
  (let [matcher (sequence-of-matcher matcher)]
    (fn [bindings data]
      (when (set? data)
        (matcher bindings (seq data))))))

(defn map-matcher* [map-entry-matchers]
  (if (seq map-entry-matchers)
    (let [[matcher & matchers] map-entry-matchers
          continuation (map-matcher* matchers)]
      (fn [bindings data]
        (cross-join (for [[k _ :as map-entry] data
                          res (matcher bindings map-entry)
                          :let [data' (dissoc data k)]]
                      (continuation res data')))))
    (epsilon)))

(defn map-matcher [map-entry-matchers]
  (let [matcher (map-matcher* map-entry-matchers)
        min-size (count map-entry-matchers)]
    (fn [bindings data]
      (when (and (map? data) (>= (count data) min-size))
        (matcher bindings data)))))

(defn map-of-matcher [map-entry-matcher]
  (let [matcher (sequence-of-matcher map-entry-matcher)]
    (fn [bindings data]
      (when (map? data)
        (matcher bindings (seq data))))))

(defn search-matcher
  ([value-matcher] (search-matcher (epsilon) value-matcher))
  ([index-matcher value-matcher]
   (let [matcher (tuple-matcher [index-matcher value-matcher])]
     (fn [bindings data]
       (cond
         (map? data)
         (cross-join (map (fn [[k value]]
                            (matcher bindings [k value]))
                          data))

         (coll? data)
         (cross-join (map-indexed (fn [i value]
                                    (matcher bindings [i value]))
                                  data))

         :else ())))))
