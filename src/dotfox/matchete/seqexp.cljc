(ns dotfox.matchete.seqexp
  (:refer-clojure :exclude [compile])
  (:require [clojure.core.async :as async]
            [dotfox.matchete.base :as base]
            [dotfox.matchete.async :refer [distinct-chan]]))

(def this-ns "dotfox.matchete.seqexp")

(defn ->seqexp-matcher [matcher]
  (base/wrap-matcher
   (fn [bindings data]
     [bindings (first data)])
   #(assoc % ::consumed 1)
   matcher))

(defn epsilon []
  (fn [bindings _data]
    (async/to-chan! [(assoc bindings ::consumed 0)])))

(defn lvar-matcher [binding]
  (fn [{::keys [consumed] :as bindings} data]
    (let [data (if consumed
                 (take consumed data)
                 (first data))
          bindings (if consumed
                     bindings
                     (assoc bindings ::consumed 1))]
      (cond
        (and (contains? bindings binding)
             (= data (get bindings binding)))
        (async/to-chan! [bindings])

        (not (contains? bindings binding))
        (async/to-chan! [(assoc bindings binding data)])

        :else (async/to-chan! [])))))

(defn mvar-matcher [binding]
  (fn [{::keys [consumed] :as bindings} data]
    (let [data (if consumed
                 (take consumed data)
                 (first data))
          bindings (if consumed
                     bindings
                     (assoc bindings ::consumed 1))]
      (async/to-chan! [(update bindings binding (fnil conj []) data)]))))

(defn and-matcher [matchers]
  (if (seq matchers)
    (let [[matcher & matchers] matchers
          continuation (and-matcher matchers)]
      (fn [bindings data]
        (let [out-ch (distinct-chan)
              port (matcher bindings data)]
          (async/go-loop [ports []]
            (if-let [res (async/<! port)]
              (recur (conj ports (continuation res data)))
              (async/pipe (async/merge ports) out-ch)))
          out-ch)))
    (epsilon)))

(defn not-matcher [matcher]
  (fn [bindings data]
    (let [port (matcher bindings data)
          out-ch (async/chan 1)]
      (async/go
        (if (async/<! port)
          (async/close! out-ch)
          (do (async/>! out-ch (assoc bindings ::consumed 0))
              (async/close! out-ch))))
      out-ch)))

(defn maybe-matcher [matcher]
  (fn [bindings data]
    (let [port1 (matcher bindings data)
          port2 (async/to-chan! [(assoc bindings ::consumed 0)])
          out-ch (distinct-chan)]
      (async/pipe (async/merge [port1 port2]) out-ch))))

(defn safe-+ [& args]
  (apply #?(:clj +' :cljs +) (keep identity args)))

(defn cat-matcher [matchers]
  (if (seq matchers)
    (let [[matcher & matchers] matchers
          continuation (cat-matcher matchers)]
      (fn [bindings data]
        (let [out-ch (distinct-chan)
              port (matcher bindings data)]
          (async/go-loop [ports []]
            (if-let [{::keys [consumed] :as res} (async/<! port)]
              (recur
               (conj ports
                     (async/pipe
                      (continuation (dissoc res ::consumed)
                                    (drop consumed data))
                      (distinct-chan (map #(update % ::consumed safe-+ consumed))))))
              (async/pipe (async/merge ports) out-ch)))
          out-ch)))
    (epsilon)))

(defn alt-matcher [matchers]
  (fn [bindings data]
    (async/merge (mapv #(% bindings data) matchers))))

(defn repeat-matcher [min max matcher]
  (let [counter-key (keyword this-ns (name (gensym)))]
    (fn repeat-matcher* [bindings data]
      (let [out-ch (if (contains? bindings counter-key)
                     (distinct-chan)
                     (distinct-chan (comp
                                     (filter #(<= min (get % counter-key)))
                                     (map #(dissoc % counter-key)))))
            bindings (update bindings counter-key (fnil inc -1))
            counter (get bindings counter-key)]
        (cond
          (and (seq data) (<= counter max))
          (async/go-loop [ports [(async/to-chan! [(update bindings ::consumed safe-+ 0)])]
                          port (matcher bindings data)]
            (if-let [{::keys [consumed] :as res} (async/<! port)]
              (recur
               (conj ports
                     (async/pipe
                      (repeat-matcher* (dissoc res ::consumed)
                                       (drop consumed data))
                      (distinct-chan (map #(update % ::consumed safe-+ consumed))))
                     (async/to-chan! [res]))
               port)
              (async/pipe (async/merge ports) out-ch)))

          (and (empty? data) (>= counter min))
          (async/go
            (async/>! out-ch (update bindings ::consumed safe-+ 0))
            (async/close! out-ch))

          :else
          (async/close! out-ch))
        out-ch))))
