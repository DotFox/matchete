(ns dotfox.matchete.basic-test
  (:require [dotfox.matchete :as m]
            [dotfox.matchete.test-helper :refer [match?]]
            #?(:clj [clojure.test :refer [deftest testing is are] :as t]
               :cljs [cljs.test :refer [deftest testing is are] :as t :include-macros true])))

(deftest lvar-binding
  (is (match? ((m/matcher [::m/? :x]) 42) #{{:x 42}}))
  (is (match? ((m/matcher [:or [::m/? :x] [::m/? :y]]) 42) #{{:x 42} {:y 42}}))

  (testing "sequence matching context"
    (let [matcher (m/matcher [:cat [::m/? :x] [::m/? :x]])]
      (are [x y] (match? (matcher x) y)
        [] #{}
        [1] #{}
        [1 2] #{}
        [1 1] #{{:x 1}}))))

(deftest mvar-binding
  (is (match? ((m/matcher [::m/! :x]) 42) #{{:x [42]}}))
  (is (match? ((m/matcher [:and [::m/! :x] [::m/! :x]]) 42) #{{:x [42 42]}}))

  (testing "sequence matching context"
    (let [matcher (m/matcher [:cat [::m/! :x] [::m/! :x]])]
      (are [x y] (match? (matcher x) y)
        [] #{}
        [1] #{}
        [1 2] #{{:x [1 2]}}))))

(deftest scan
  (let [pattern [::m/scan [:and int? [::m/? :x]]]
        matcher (m/matcher pattern)]
    (are [x y] (match? (matcher x) y)
      42 #{}
      [] #{}
      [42] #{{:x 42}}
      [42 42] #{{:x 42}}
      [42 43] #{{:x 42} {:x 43}}
      [42 "42" 43] #{{:x 42} {:x 43}}))

  (testing "sequence matching context"
    (let [pattern [:cat [::m/scan [:and int? [::m/? :x]]] :string?]
        matcher (m/matcher pattern)]
    (are [x y] (match? (matcher x) y)
      [42 ""] #{}
      [[] ""] #{}
      [[42] ""] #{{:x 42}}
      [[42 42] ""] #{{:x 42}}
      [[42 43] ""] #{{:x 42} {:x 43}}
      [[42 "42" 43] ""] #{{:x 42} {:x 43}}))))
