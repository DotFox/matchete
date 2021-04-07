(ns dotfox.matchete.test-helper
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            #?(:clj [clojure.core.async :as async]
               :cljs [cljs.core.async :as async :include-macros true])))

(defn match-result [actual expected]
  (t/is (not= :timeout! actual))
  (t/is (= (set expected) (set actual))))

(defn test-async [ch expected]
  #?(:clj (match-result (async/<!! ch) expected)
     :cljs (t/async done
                    (async/take! ch (fn [result]
                                      (match-result result expected)
                                      (done))))))

(defmacro target [& {:keys [cljs clj]}]
  (if (contains? &env '&env)
    `(if (:ns ~'&env) ~cljs ~clj)
    (if #?(:clj (:ns &env) :cljs true)
      cljs
      clj)))

(defmacro is-match? [form expected & [timeout]]
  `(let [result-chan# ~form
         timeout-chan#
         (~(target :clj 'clojure.core.async/timeout :cljs 'cljs.core.async/timeout)
          (or ~timeout 5000))]
     (test-async
      (~(target :clj 'clojure.core.async/go-loop :cljs 'cljs.core.async/go-loop)
       [results# []]
       (let [[res# port#] (~(target :clj 'clojure.core.async/alts! :cljs 'cljs.core.async/alts!)
                           [result-chan# timeout-chan#])]
         (if (some? res#)
           (if (= port# timeout-chan#)
             (do
               (~(target :clj 'clojure.core.async/close! :cljs 'cljs.core.async/close!)
                result-chan#)
               :timeout!)
             (recur (conj results# res#)))
           results#)))
      ~expected)))

(defmacro are-match?
  {:style/indent 2}
  [argv expr & args]
  (if (or
       ;; (are [] true) is meaningless but ok
       (and (empty? argv) (empty? args))
       ;; Catch wrong number of args
       (and (pos? (count argv))
            (pos? (count args))
            (zero? (mod (count args) (count argv)))))
    `(clojure.template/do-template ~argv (is-match? ~@expr) ~@args)
    (throw (#?(:clj Exception. :cljs js/Error.) "The number of args doesn't match are's argv."))))
