(ns dotfox.matchete.basic-test
  (:require [dotfox.matchete :as m]
            [dotfox.matchete.test-helper :refer [is-match? are-match?]
             #?@(:cljs [:include-macros true])]
            #?(:clj [clojure.test :refer [deftest testing] :as t]
               :cljs [cljs.test :refer [deftest testing] :as t :include-macros true])))

(deftest lvar-binding
  (is-match? ((m/matcher [:lvar :x]) 41) #{{:x 41}})
  (is-match? ((m/matcher [:or [:lvar :x] [:lvar :y]]) 42) #{{:x 42} {:y 42}})

  (testing "sequence matching context"
    (let [matcher (m/matcher [:cat [:lvar :x] [:lvar :x]])]
      (are-match? [x y] ((matcher x) y)
        [] #{}
        [1] #{}
        [1 2] #{}
        [1 1] #{{:x 1}}))))

(deftest mvar-binding
  (is-match? ((m/matcher [:mvar :x]) 42) #{{:x [42]}})
  (is-match? ((m/matcher [:and [:mvar :x] [:mvar :x]]) 42) #{{:x [42 42]}})

  (testing "sequence matching context"
    (let [matcher (m/matcher [:cat [:mvar :x] [:mvar :x]])]
      (are-match? [x y] ((matcher x) y)
        [] #{}
        [1] #{}
        [1 2] #{{:x [1 2]}}))))

(deftest scan
  (let [pattern [:search [:and int? [:lvar :x]]]
        matcher (m/matcher pattern)]
    (are-match? [x y] ((matcher x) y)
      42 #{}
      [] #{}
      [42] #{{:x 42}}
      [42 42] #{{:x 42}}
      [42 43] #{{:x 42} {:x 43}}
      [42 "42" 43] #{{:x 42} {:x 43}}))

  (testing "sequence matching context"
    (let [pattern [:cat [:search [:and int? [:lvar :x]]] :string?]
          matcher (m/matcher pattern)]
      (are-match? [x y] ((matcher x) y)
        [42 ""] #{}
        [[] ""] #{}
        [[42] ""] #{{:x 42}}
        [[42 42] ""] #{{:x 42}}
        [[42 43] ""] #{{:x 42} {:x 43}}
        [[42 "42" 43] ""] #{{:x 42} {:x 43}}))))
