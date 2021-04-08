(ns dotfox.matchete.seqex-test
  (:require [dotfox.matchete :as m]
            #?(:clj [clojure.test :refer [deftest is] :as t]
               :cljs [cljs.test :refer [deftest is] :as t :include-macros true])))

(deftest basic
  (is (= [{:pre ["A" "A" "A" "A" "A" "A" "A" "A" "A"], :post ["A"]}
          {:pre ["A" "A" "A" "A" "A" "A" "A" "A"], :post ["A" "A"]}
          {:pre ["A" "A" "A" "A" "A" "A" "A"], :post ["A" "A" "A"]}
          {:pre ["A" "A" "A" "A" "A" "A"], :post ["A" "A" "A" "A"]}
          {:pre ["A" "A" "A" "A" "A"], :post ["A" "A" "A" "A" "A"]}
          {:pre ["A" "A" "A" "A"], :post ["A" "A" "A" "A" "A" "A"]}
          {:pre ["A" "A" "A"], :post ["A" "A" "A" "A" "A" "A" "A"]}
          {:pre ["A" "A"], :post ["A" "A" "A" "A" "A" "A" "A" "A"]}
          {:pre ["A"], :post ["A" "A" "A" "A" "A" "A" "A" "A" "A"]}
          {:post ["A" "A" "A" "A" "A" "A" "A" "A" "A" "A"]}]
         ((m/matcher [:cat
                      [:* [:and [:= "A"] [:mvar :pre]]]
                      [:+ [:and [:= "A"] [:mvar :post]]]])
          (repeat 10 "A")))))
