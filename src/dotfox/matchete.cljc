(ns dotfox.matchete
  (:require [dotfox.matchete.compiler :as compiler]))

(defn matcher
  ([pattern] (matcher nil pattern))
  ([registry pattern]
   (let [matcher (compiler/compile registry pattern)]
     (fn
       ([data] (matcher {} data))
       ([bindings data] (matcher bindings data))))))
