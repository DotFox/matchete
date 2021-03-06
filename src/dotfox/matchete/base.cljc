(ns dotfox.matchete.base)

(defn mapcat-distinct [map-fn coll]
  (sequence
    (comp
      (mapcat map-fn)
      (distinct))
    coll))

(defn epsilon []
  (fn [bindings _data]
    (list bindings)))

(defn lvar-matcher [binding]
  (fn [bindings data]
    (cond
      (and (contains? bindings binding)
           (= data (get bindings binding)))
      (list bindings)

      (not (contains? bindings binding))
      (list (assoc bindings binding data))

      :else nil)))

(defn mvar-matcher [binding]
  (fn [bindings data]
    (list (update bindings binding (fnil conj []) data))))

(defn wrap-matcher [{:keys [entry exit]
                     :or {entry list
                          exit list}}
                    matcher]
  (fn [bindings data]
    (sequence
     (comp (mapcat exit)
           (keep identity))
     (apply matcher (entry bindings data)))))

(defn and-matcher [matchers]
  (if (seq matchers)
    (let [[matcher & matchers] matchers
          continuation (and-matcher matchers)]
      (fn [bindings data]
        (mapcat-distinct #(continuation % data) (matcher bindings data))))
    (fn [bindings _data]
      (list bindings))))

(defn or-matcher [matchers]
  (fn [bindings data]
    (mapcat-distinct #(% bindings data) matchers)))

(defn orn-matcher [branch-key matchers]
  (let [matchers (mapv
                  (fn [[branch-value matcher]]
                    (wrap-matcher {:exit (fn [bindings]
                                           [(assoc bindings branch-key branch-value)])}
                                  matcher))
                  matchers)]
    (or-matcher matchers)))

(defn not-matcher [matcher]
  (fn [bindings data]
    (when-not (first (matcher bindings data))
      (list bindings))))

(defn maybe-matcher [matcher]
  (fn [bindings data]
    (cons bindings (matcher bindings data))))

(defn regex-matcher [regex]
  (fn [bindings data]
    (when (and (string? data) (re-matches regex data))
      (list bindings))))

(defn multi-matcher [dispatch-fn matchers]
  (fn [bindings data]
    (let [dispatch-value (dispatch-fn data)]
      (when-let [matcher (get matchers dispatch-value)]
        (matcher bindings data)))))

(defn pred-matcher [pred]
  (fn [bindings data]
    (when (pred data)
      (list bindings))))

(defn function-matcher [f]
  (fn [bindings data]
    (when-let [res (seq (f bindings data))]
      (cond
        (map? res) (list res)
        (sequential? res) res))))
