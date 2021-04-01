(ns dotfox.matchete.test-helper
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            #?(:clj [clojure.core.async :as async]
               :cljs [cljs.core.async :as async :include-macros true])))

(defn match-result [actual expected]
  (t/is (not= :timeout! actual))
  (t/is (= (set actual) (set expected))))

(defn test-async [ch expected]
  #?(:clj (match-result (async/<!! ch) expected)
     :cljs (t/async done
                    (async/take! ch (fn [result]
                                      (match-result result expected)
                                      (done))))))

(defmacro is-match? [form expected & [timeout]]
  `(let [result-chan# ~form
         timeout-chan# (~(if (contains? &env :js-globals)
                           'cljs.core.async/timeout
                           'clojure.core.async/timeout)
                        (or ~timeout 5000))]
     (test-async
      (~(if (contains? &env :js-globals)
          'cljs.core.async/go-loop
          'clojure.core.async/go-loop)
       [results# []]
       (let [[res# port#] (~(if (contains? &env :js-globals)
                              'cljs.core.async/alts!
                              'clojure.core.async/alts!)
                           [result-chan# timeout-chan#])]
         (if (some? res#)
           (if (= port# timeout-chan#)
             (do
               (~(if (contains? &env :js-globals)
                   'cljs.core.async/close!
                   'clojure.core.async/close!)
                result-chan#)
               :timeout!)
             (recur (conj results# res#)))
           results#)))
      ~expected)))

(defmacro are-match? [argv expr & args]
  (if (or
       ;; (are [] true) is meaningless but ok
       (and (empty? argv) (empty? args))
       ;; Catch wrong number of args
       (and (pos? (count argv))
            (pos? (count args))
            (zero? (mod (count args) (count argv)))))
    `(clojure.template/do-template ~argv (is-match? ~@expr) ~@args)
    (throw (#?(:clj Exception. :cljs js/Error.) "The number of args doesn't match are's argv."))))
