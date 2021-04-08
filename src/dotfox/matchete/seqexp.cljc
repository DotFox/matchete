(ns dotfox.matchete.seqexp
  (:require [dotfox.matchete.base :as base :refer [cross-join]]))

(def this-ns "dotfox.matchete.seqexp")

(defn safe+ [a b]
  (+ (or a 0) (or b 0)))

(defn ->seqexp-matcher [matcher]
  (base/wrap-matcher
   {:entry (fn [{::keys [consumed] :as bindings} data]
             [bindings (first (if consumed
                                (drop consumed data)
                                data))])
    :exit (fn [bindings]
            (list (update bindings ::consumed safe+ 1)))}
   matcher))

(defn epsilon []
  (fn [bindings _data]
    (list (update bindings ::consumed safe+ 0))))

(defn and-matcher [matchers]
  (if (seq matchers)
    (let [matcher (first matchers)
          continuation (when-let [matchers (seq (rest matchers))]
                         (and-matcher matchers))]
      (fn [bindings data]
        (cross-join (for [res (matcher bindings data)]
                      (if continuation
                        (continuation (dissoc res ::consumed) data)
                        (list res))))))
    (fn [bindings _data]
      (list bindings))))

(defn cat-matcher [matchers]
  (if (seq matchers)
    (let [[matcher & matchers] matchers
          continuation (cat-matcher matchers)]
      (fn [{::keys [consumed] :as bindings} data]
        (cross-join
         (for [res (matcher (dissoc bindings ::consumed) data)]
           (continuation
            (update res ::consumed safe+ consumed)
            (if-let [consumed (get res ::consumed)]
              (drop consumed data)
              data))))))
    (epsilon)))

(defn alt-matcher [matchers]
  (base/or-matcher matchers))

(defn altn-matcher [branch-key matchers]
  (base/orn-matcher branch-key matchers))

(defn repeat-matcher [min max matcher]
  (let [counter-key (keyword this-ns (name (gensym)))]
    (fn repeat-matcher* [{::keys [consumed] :as bindings} data]
      (if-let [counter (get bindings counter-key)]
        (cond

          (empty? data)
          (when (<= min counter max)
            (list bindings))

          (= counter max)
          (list bindings)

          (< counter max)
          (let [results (cross-join
                         (for [res (matcher (dissoc bindings ::consumed) data)
                               :let [consumed' (get res ::consumed)]]
                           (repeat-matcher*
                            (-> res
                                (update counter-key inc)
                                (update ::consumed safe+ consumed))
                            (drop consumed' data))))]
            (if (>= counter min)
              (cons bindings results)
              results)))
        (sequence
         (map #(dissoc % counter-key))
         (repeat-matcher*
          (assoc bindings
                 counter-key 0
                 ::consumed 0)
          data))))))

(comment

  ((cat-matcher [(->seqexp-matcher (base/lvar-matcher :x))
                 (->seqexp-matcher (base/lvar-matcher :y))])
   {} [1 2])

  ((cat-matcher [(and-matcher [(->seqexp-matcher (base/pred-matcher #(= % "A")))
                               (->seqexp-matcher (base/mvar-matcher :pre))])])
   {} ["A" "A"])

  )
