(ns dotfox.matchete.async
  (:require [clojure.core.async :as async]))

(defn distinct-chan
  ([] (distinct-chan nil))
  ([xform]
   (async/chan (async/buffer 1024) (if xform (comp (distinct) xform) (distinct)))))
