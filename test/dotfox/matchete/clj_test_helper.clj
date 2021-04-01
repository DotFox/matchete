(ns dotfox.matchete.clj-test-helper
  (:require [clojure.test :as t]
            [clojure.core.async :as async]))

(defn test-async [ch]
  (async/<!! ch))

(defmethod t/assert-expr 'match? [msg form]
  `(let [chan# ~(second form)
         timeout# ~(or (nth form 3 nil) 5000)
         timeout-chan# (async/timeout timeout#)
         expected# ~(nth form 2)
         worker# (async/go-loop [expected-# expected#
                                 actual# #{}]
                   (if-let [res# (async/<! chan#)]
                     (if (contains? expected-# res#)
                       (recur (disj expected-# res#) (conj actual# res#))
                       (t/do-report {:type :fail :message ~msg
                                     :expected expected-# :actual res#}))
                     (if (empty? expected-#)
                       (t/do-report {:type :pass :message ~msg
                                     :expected expected# :actual actual#})
                       (t/do-report {:type :fail :message ~msg
                                     :expected expected# :actual actual#}))))]
     (test-async (async/go
                   (let [[_# port#] (async/alts! [timeout-chan# worker#])]
                     (when (= port# timeout-chan#)
                       (async/close! chan#)
                       (async/close! worker#)
                       (t/do-report {:type :fail :message "Timeout!"})))))))
