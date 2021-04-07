(ns dotfox.matchete.base
  (:require [clojure.core.async :as async]
            [dotfox.matchete.async :refer [distinct-chan]]))

(defn epsilon []
  (fn [bindings _data]
    (async/to-chan! [bindings])))

(defn lvar-matcher [binding]
  (fn [bindings data]
    (cond
      (and (contains? bindings binding)
           (= data (get bindings binding)))
      (async/to-chan! [bindings])

      (not (contains? bindings binding))
      (async/to-chan! [(assoc bindings binding data)])

      :else (async/to-chan! []))))

(defn mvar-matcher [binding]
  (fn [bindings data]
    (async/to-chan! [(update bindings binding (fnil conj []) data)])))

(defn wrap-matcher [in-fn out-fn matcher]
  (fn [bindings data]
    (let [[bindings data] (in-fn bindings data)]
      (async/pipe (matcher bindings data)
                  (distinct-chan (comp (mapcat (fn [result]
                                                 (let [result (out-fn result)]
                                                   (if (map? result) [result] result))))
                                       (keep identity)))))))

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

(defn or-matcher [matchers]
  (fn [bindings data]
    (async/pipe (async/merge (mapv #(% bindings data) matchers)) (distinct-chan))))

(defn orn-matcher [branch-key matchers]
  (let [matchers (mapv (fn [[branch matcher]]
                         (wrap-matcher vector #(assoc % branch-key branch) matcher))
                       matchers)]
    (or-matcher matchers)))

(defn not-matcher [matcher]
  (fn [bindings data]
    (let [port (matcher bindings data)
          out-ch (async/chan 1)]
      (async/go
        (if (async/<! port)
          (async/close! out-ch)
          (do (async/>! out-ch bindings)
              (async/close! out-ch))))
      out-ch)))

(defn maybe-matcher [matcher]
  (fn [bindings data]
    (let [port1 (matcher bindings data)
          port2 (async/to-chan! [bindings])
          out-ch (distinct-chan)]
      (async/pipe (async/merge [port1 port2]) out-ch))))

(defn regex-matcher [regex]
  (fn [bindings data]
    (async/to-chan!
     (when (and (string? data) (re-matches regex data))
       [bindings]))))

(defn multi-matcher [dispatch-fn matchers]
  (fn [bindings data]
    (let [dispatch-value (dispatch-fn data)]
      (if-let [matcher (get matchers dispatch-value)]
        (matcher bindings data)
        (async/to-chan! nil)))))

(defn pred-matcher [pred]
  (fn [bindings data]
    (async/to-chan! (when (pred data) [bindings]))))

(defn function-matcher [f]
  (fn [bindings data]
    (async/to-chan! (f bindings data))))
