(ns dotfox.matchete.cljs-test-helper
  (:require [cljs.test :as t]
            [cljs.core.async :as async]))

#?(:cljs
   (defn test-async [ch]
     (t/async done (async/take! ch (fn [_] (done))))))

#?(:clj
   (defmethod t/assert-expr 'match? [_ msg form]
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
        (dotfox.matchete.cljs-test-helper/test-async
         (async/go
           (let [[_# port#] (async/alts! [timeout-chan# worker#])]
             (when (= port# timeout-chan#)
               (async/close! chan#)
               (async/close! worker#)
               (t/do-report {:type :fail :message "Timeout!"}))))))))