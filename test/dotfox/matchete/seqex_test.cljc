(ns dotfox.matchete.seqex-test
  (:require [dotfox.matchete :as m]
            [dotfox.matchete.test-helper :refer [is-match?]
             #?@(:cljs [:include-macros true])]
            #?(:clj [clojure.test :refer [deftest] :as t]
               :cljs [cljs.test :refer [deftest] :as t :include-macros true])))

(deftest basic
  (is-match? ((m/matcher [:cat
                          [:and [:* [:= "A"]] [::m/? :pre]]
                          [:and [:+ [:= "A"]] [::m/? :post]]])
              (repeat 10 "A"))
             #{{:pre () :post '("A" "A" "A" "A" "A" "A" "A" "A" "A" "A")}
               {:pre '("A") :post '("A" "A" "A" "A" "A" "A" "A" "A" "A")}
               {:pre '("A" "A") :post '("A" "A" "A" "A" "A" "A" "A" "A")}
               {:pre '("A" "A" "A") :post '("A" "A" "A" "A" "A" "A" "A")}
               {:pre '("A" "A" "A" "A") :post '("A" "A" "A" "A" "A" "A")}
               {:pre '("A" "A" "A" "A" "A") :post '("A" "A" "A" "A" "A")}
               {:pre '("A" "A" "A" "A" "A" "A") :post '("A" "A" "A" "A")}
               {:pre '("A" "A" "A" "A" "A" "A" "A") :post '("A" "A" "A")}
               {:pre '("A" "A" "A" "A" "A" "A" "A" "A") :post '("A" "A")}
               {:pre '("A" "A" "A" "A" "A" "A" "A" "A" "A") :post '("A")}}))
