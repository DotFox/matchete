(ns dotfox.matchete.collection
  (:require [clojure.core.async :as async]
            [dotfox.matchete.async :refer [distinct-chan]]
            [dotfox.matchete.base :refer [epsilon]]))

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
              (async/pipe (async/merge ports) out-ch)))
          out-ch)))
    (epsilon)))

(defn sequence-of-matcher [matcher]
  (fn sequence-of-matcher* [bindings data]
    (if (seq data)
      (let [out-ch (distinct-chan)
            port (matcher bindings (first data))]
        (async/go-loop [ports []]
          (if-let [res (async/<! port)]
            (recur (conj ports (sequence-of-matcher* res (rest data))))
            (async/pipe (async/merge ports) out-ch)))
        out-ch)
      (async/to-chan! [bindings]))))

(defn tuple-matcher [matchers]
  (let [size (count matchers)
        matcher (sequence-matcher matchers)]
    (fn [bindings data]
      (if (= size (count data))
        (matcher bindings data)
        (async/to-chan! [])))))

(defn- set-matcher* [matchers]
  (if (seq matchers)
    (let [[matcher & matchers] matchers
          continuation (set-matcher* matchers)
          step-fn (fn [bindings data el]
                    (let [port (matcher bindings el)
                          out-ch (distinct-chan)]
                      (async/go-loop [ports []]
                        (if-let [res (async/<! port)]
                          (recur (conj ports (continuation res (disj data el))))
                          (async/pipe (async/merge ports) out-ch)))
                      out-ch))]
      (fn [bindings data]
        (let [ports (mapv #(step-fn bindings data %) data)]
          (async/pipe (async/merge ports) (distinct-chan)))))
    (epsilon)))

(defn set-matcher [matchers]
  (let [min-size (count matchers)
        set-matcher* (set-matcher* matchers)]
    (fn [bindings data]
      (if (and (set? data) (>= (count data) min-size))
        (set-matcher* bindings data)
        (async/to-chan! [])))))

(defn set-of-matcher [matcher]
  (fn set-of* [bindings data]
    (if ((every-pred set? not-empty) data)
      (let [out-ch (distinct-chan)
            el (first data)
            port (matcher bindings el)]
        (async/go-loop [ports []]
          (if-let [res (async/<! port)]
            (recur (conj ports (set-of* res (disj data el))))
            (async/pipe (async/merge ports) out-ch)))
        out-ch)
      (async/to-chan! [bindings]))))

(defn- map-matcher* [map-entry-matchers]
  (if (seq map-entry-matchers)
    (fn [bindings data]
      (let [ports (for [[k _ :as kv] data
                        map-entry-matcher map-entry-matchers
                        :let [data' (dissoc data k)
                              continuation (map-matcher* (disj map-entry-matchers
                                                               map-entry-matcher))
                              out-ch (distinct-chan)
                              port (map-entry-matcher bindings kv)]]
                    (do
                      (async/go-loop [ports []]
                        (if-let [res (async/<! port)]
                          (recur (conj ports (continuation res data')))
                          (async/pipe (async/merge ports) out-ch)))
                      out-ch))]
        (async/merge (vec ports))))
    (epsilon)))

(defn map-matcher [map-entry-matchers]
  (let [min-size (count map-entry-matchers)
        map-matcher* (map-matcher* (set map-entry-matchers))]
    (fn [bindings data]
      (if (and (map? data) (>= (count data) min-size))
        (map-matcher* bindings data)
        (async/to-chan! [])))))

(defn map-of-matcher [map-entry-matcher]
  (fn map-of-matcher* [bindings data]
    (if (map? data)
      (if-let [[k _ :as map-entry] (first data)]
        (let [out-ch (distinct-chan)
              port (map-entry-matcher bindings map-entry)]
          (async/go-loop [ports []]
            (if-let [res (async/<! port)]
              (recur (conj ports (map-of-matcher* res (dissoc data k))))
              (async/pipe (async/merge ports) out-ch)))
          out-ch)
        (async/to-chan! [bindings]))
      (async/to-chan! nil))))

(defn search-matcher [matcher]
  (fn [bindings data]
    (if (and (sequential? data) (seq data))
      (async/pipe (async/merge (mapv #(matcher bindings %) data)) (distinct-chan))
      (async/to-chan! []))))
