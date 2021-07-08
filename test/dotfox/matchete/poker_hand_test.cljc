(ns dotfox.matchete.poker-hand-test
  (:require
   [dotfox.matchete :as m]
   #?(:clj [clojure.test :refer [are deftest] :as t]
      :cljs [cljs.test :refer [are deftest] :as t :include-macros true])))

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
    (m/matcher
      [:orn {:branch-key :hand}
       [:strait-flush [:set
                       [:tuple [:lvar :suit] [:fn (plus-match-fn :rank)]]
                       [:tuple [:lvar :suit] [:fn (plus-match-fn :rank 1)]]
                       [:tuple [:lvar :suit] [:fn (plus-match-fn :rank 2)]]
                       [:tuple [:lvar :suit] [:fn (plus-match-fn :rank 3)]]
                       [:tuple [:lvar :suit] [:fn (plus-match-fn :rank 4)]]]]
       [:strait [:set
                 [:tuple any? [:fn (plus-match-fn :rank)]]
                 [:tuple any? [:fn (plus-match-fn :rank 1)]]
                 [:tuple any? [:fn (plus-match-fn :rank 2)]]
                 [:tuple any? [:fn (plus-match-fn :rank 3)]]
                 [:tuple any? [:fn (plus-match-fn :rank 4)]]]]
       [:royal-flush [:set
                      [:tuple [:lvar :suit] [:= 10]]
                      [:tuple [:lvar :suit] [:= 11]]
                      [:tuple [:lvar :suit] [:= 12]]
                      [:tuple [:lvar :suit] [:= 13]]
                      [:tuple [:lvar :suit] [:= 14]]]]
       [:four-of-kind [:set
                       any?
                       [:tuple any? [:lvar :rank]]
                       [:tuple any? [:lvar :rank]]
                       [:tuple any? [:lvar :rank]]
                       [:tuple any? [:lvar :rank]]]]
       [:three-of-kind [:set
                        any?
                        any?
                        [:tuple any? [:lvar :rank]]
                        [:tuple any? [:lvar :rank]]
                        [:tuple any? [:lvar :rank]]]]
       [:full-house [:set
                     [:tuple any? [:lvar :full]]
                     [:tuple any? [:lvar :full]]
                     [:tuple any? [:lvar :full]]
                     [:tuple any? [:lvar :house]]
                     [:tuple any? [:lvar :house]]]]
       [:pair [:set
               any?
               any?
               any?
               [:tuple any? [:lvar :rank]]
               [:tuple any? [:lvar :rank]]]]
       [:two-pairs [:set
                    any?
                    [:tuple any? [:lvar :rank-1]]
                    [:tuple any? [:lvar :rank-1]]
                    [:tuple any? [:lvar :rank-2]]
                    [:tuple any? [:lvar :rank-2]]]]
       [:flush [:set-of [:tuple [:lvar :suit] any?]]]
       [:highest-card [:set-of [:tuple any? [:fn high-card]]]]])))

(deftest poker
  (are [hand expected] (= expected (poker-hand-matcher hand))
    #{[:♠ 5] [:♦ 11] [:♠ 6] [:♠ 7] [:♠ 8]}
    [{:rank 11 :hand :highest-card}]

    #{[:♠ 5] [:♦ 10] [:♠ 7] [:♣ 5] [:♥ 8]}
    [{:rank 5 :hand :pair}
     {:rank 10 :hand :highest-card}]

    #{[:♠ 5] [:♦ 10] [:♠ 7] [:♣ 5] [:♥ 10]}
    [{:hand :pair :rank 10}
     {:hand :pair :rank 5}
     {:hand :two-pairs :rank-1 5 :rank-2 10}
     {:hand :two-pairs :rank-1 10 :rank-2 5}
     {:hand :highest-card :rank 10}]

    #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 8]}
    [{:rank 5 :hand :pair}
     {:rank 5 :hand :three-of-kind}
     {:rank 8 :hand :highest-card}]

    #{[:♠ 5] [:♣ 6] [:♠ 7] [:♠ 8] [:♠ 9]}
    [{:rank 9 :hand :highest-card}
     {:rank 5 :hand :strait}]

    #{[:♠ 5] [:♠ 6] [:♠ 7] [:♠ 13] [:♠ 9]}
    [{:suit :♠ :hand :flush}
     {:rank 13 :hand :highest-card}]

    #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 7]}
    [{:full 5 :hand :full-house :house 7}
     {:hand :pair :rank 5}
     {:hand :pair :rank 7}
     {:hand :three-of-kind :rank 5}
     {:hand :two-pairs :rank-1 7 :rank-2 5}
     {:hand :two-pairs :rank-1 5 :rank-2 7}
     {:hand :highest-card :rank 7}]

    #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 5]}
    [{:rank 5 :hand :pair}
     {:rank 5 :hand :three-of-kind}
     {:rank-1 5 :rank-2 5 :hand :two-pairs}
     {:rank 7 :hand :highest-card}
     {:rank 5 :hand :four-of-kind}]

    #{[:♠ 5] [:♠ 6] [:♠ 7] [:♠ 8] [:♠ 9]}
    [{:suit :♠ :hand :flush}
     {:suit :♠ :rank 5 :hand :strait-flush}
     {:rank 9 :hand :highest-card}
     {:rank 5 :hand :strait}]))
