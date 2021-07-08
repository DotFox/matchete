(ns dotfox.matchete.basic-test
  (:require
   [dotfox.matchete :as m]
   #?(:clj [clojure.test :refer [are deftest is testing] :as t]
      :cljs [cljs.test :refer [are deftest is testing] :as t :include-macros true])))

(deftest lvar-binding
  (is (= [{:x 41}] ((m/matcher [:lvar :x]) 41)))
  (is (= [{:x 42} {:y 42}] ((m/matcher [:or [:lvar :x] [:lvar :y]]) 42)))

  (testing "sequence matching context"
    (let [matcher (m/matcher [:cat [:lvar :x] [:lvar :x]])]
      (are [x y] (= y (matcher x))
        [] []
        [1] []
        [1 2] []
        [1 1] [{:x 1}]))))

(deftest mvar-binding
  (is (= [{:x [42]}] ((m/matcher [:mvar :x]) 42)))
  (is (= [{:x [42 42]}] ((m/matcher [:and [:mvar :x] [:mvar :x]]) 42)))

  (testing "sequence matching context"
    (let [matcher (m/matcher [:cat [:mvar :x] [:mvar :x]])]
      (are [x y] (= y (matcher x))
        [] []
        [1] []
        [1 2] [{:x [1 2]}]))))

(deftest scan
  (let [pattern [:search [:and int? [:lvar :x]]]
        matcher (m/matcher pattern)]
    (are [x y] (= y (matcher x))
      42 []
      [] []
      [42] [{:x 42}]
      [42 42] [{:x 42}]
      [42 43] [{:x 42} {:x 43}]
      [42 "42" 43] [{:x 42} {:x 43}]))

  (testing "sequence matching context"
    (let [pattern [:cat [:search [:and int? [:lvar :x]]] :string?]
          matcher (m/matcher pattern)]
      (are [x y] (= y (matcher x))
        [42 ""] []
        [[] ""] []
        [[42] ""] [{:x 42}]
        [[42 42] ""] [{:x 42}]
        [[42 43] ""] [{:x 42} {:x 43}]
        [[42 "42" 43] ""] [{:x 42} {:x 43}]))))
