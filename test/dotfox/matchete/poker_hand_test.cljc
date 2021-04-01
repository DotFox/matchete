(ns dotfox.matchete.poker-hand-test
  (:require [dotfox.matchete :as m]
            [dotfox.matchete.test-helper :refer [are-match?] :include-macros true]
            #?(:clj [clojure.test :refer [deftest] :as t]
               :cljs [cljs.test :refer [deftest] :as t :include-macros true])))

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
       [:orn {:key :hand}
        [:strait-flush [:set
                        [:tuple [::m/? :suit] [::m/fn (plus-match-fn :rank)]]
                        [:tuple [::m/? :suit] [::m/fn (plus-match-fn :rank 1)]]
                        [:tuple [::m/? :suit] [::m/fn (plus-match-fn :rank 2)]]
                        [:tuple [::m/? :suit] [::m/fn (plus-match-fn :rank 3)]]
                        [:tuple [::m/? :suit] [::m/fn (plus-match-fn :rank 4)]]]]
        [:strait [:set
                  [:tuple any? [::m/fn (plus-match-fn :rank)]]
                  [:tuple any? [::m/fn (plus-match-fn :rank 1)]]
                  [:tuple any? [::m/fn (plus-match-fn :rank 2)]]
                  [:tuple any? [::m/fn (plus-match-fn :rank 3)]]
                  [:tuple any? [::m/fn (plus-match-fn :rank 4)]]]]
        [:royal-flush [:set
                       [:tuple [::m/? :suit] [:= 10]]
                       [:tuple [::m/? :suit] [:= 11]]
                       [:tuple [::m/? :suit] [:= 12]]
                       [:tuple [::m/? :suit] [:= 13]]
                       [:tuple [::m/? :suit] [:= 14]]]]
        [:four-of-kind [:set
                        any?
                        [:tuple any? [::m/? :rank]]
                        [:tuple any? [::m/? :rank]]
                        [:tuple any? [::m/? :rank]]
                        [:tuple any? [::m/? :rank]]]]
        [:three-of-kind [:set
                         any?
                         any?
                         [:tuple any? [::m/? :rank]]
                         [:tuple any? [::m/? :rank]]
                         [:tuple any? [::m/? :rank]]]]
        [:full-house [:set
                      [:tuple any? [::m/? :full]]
                      [:tuple any? [::m/? :full]]
                      [:tuple any? [::m/? :full]]
                      [:tuple any? [::m/? :house]]
                      [:tuple any? [::m/? :house]]]]
        [:pair [:set
                any?
                any?
                any?
                [:tuple any? [::m/? :rank]]
                [:tuple any? [::m/? :rank]]]]
        [:two-pairs [:set
                     any?
                     [:tuple any? [::m/? :rank-1]]
                     [:tuple any? [::m/? :rank-1]]
                     [:tuple any? [::m/? :rank-2]]
                     [:tuple any? [::m/? :rank-2]]]]
        [:flush [:set-of [:tuple [::m/? :suit] any?]]]
        [:highest-card [:set-of [:tuple any? [::m/fn high-card]]]]])))

(deftest poker
  (are-match? [hand expected] ((poker-hand-matcher hand) expected)
    #{[:♠ 5] [:♦ 11] [:♠ 6] [:♠ 7] [:♠ 8]}
    #{{:rank 11, :hand :highest-card}}

    #{[:♠ 5] [:♦ 10] [:♠ 7] [:♣ 5] [:♥ 8]}
    #{{:rank 5, :hand :pair}
      {:rank 10, :hand :highest-card}}

    #{[:♠ 5] [:♦ 10] [:♠ 7] [:♣ 5] [:♥ 10]}
    #{{:rank-1 5, :rank-2 10, :hand :two-pairs}
      {:rank-1 10, :rank-2 5, :hand :two-pairs}
      {:rank 10, :hand :pair}
      {:rank 5 :hand :pair}
      {:rank 10, :hand :highest-card}}

    #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 8]}
    #{{:rank 5, :hand :pair}
      {:rank 5, :hand :three-of-kind}
      {:rank 8, :hand :highest-card}}

    #{[:♠ 5] [:♣ 6] [:♠ 7] [:♠ 8] [:♠ 9]}
    #{{:rank 9, :hand :highest-card}
      {:rank 5, :hand :strait}}

    #{[:♠ 5] [:♠ 6] [:♠ 7] [:♠ 13] [:♠ 9]}
    #{{:suit :♠, :hand :flush}
      {:rank 13, :hand :highest-card}}

    #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 7]}
    #{{:rank 7, :hand :pair}
      {:rank 5, :hand :pair}
      {:rank 5, :hand :three-of-kind}
      {:rank-1 7, :rank-2 5, :hand :two-pairs}
      {:rank 7, :hand :highest-card}
      {:rank-1 5, :rank-2 7, :hand :two-pairs}
      {:full 5, :house 7, :hand :full-house}}

    #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 5]}
    #{{:rank 5, :hand :pair}
      {:rank 5, :hand :three-of-kind}
      {:rank 5, :hand :four-of-kind}
      {:rank-1 5, :rank-2 5, :hand :two-pairs}
      {:rank 7, :hand :highest-card}}

    #{[:♠ 5] [:♠ 6] [:♠ 7] [:♠ 8] [:♠ 9]}
    #{{:suit :♠, :hand :flush}
      {:rank 9, :hand :highest-card}
      {:suit :♠, :rank 5, :hand :strait-flush}
      {:rank 5, :hand :strait}}))
