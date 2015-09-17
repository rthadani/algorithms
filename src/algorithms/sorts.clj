(ns algorithms.sorts)

(defn quick-sort
  [array]
  (if-not (seq array)
    []
    (concat
      (quick-sort (filter #(<= % (first array)) (rest array)))
      [(first array)]
      (quick-sort (filter #(> % (first array)) (rest array))))))

(defn- insert
  [element result]
  (cond
    (empty? result) [element]
    (< element (first result)) (cons element result)
    :else (cons (first result) (insert element (rest result)))))

(defn insertion-sort
  [array]
  (loop [acc      []
         elements array]
    (if (empty? elements)
      acc
      (recur (insert (first elements) acc) (rest elements)))))

(def to-sort (shuffle (range 0 100)))