(ns dotfox.matchete.test-helper
  (:require #?(:clj [dotfox.matchete.clj-test-helper]
               :cljs [dotfox.matchete.cljs-test-helper])))

(declare ^{:arglists '([channel-form expected & [timeout]])} match?)
