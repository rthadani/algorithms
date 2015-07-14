(ns algorithms.sorts)

(defn quick-sort
  [array]
  (if-not (seq array)
    []
    (concat
      (quick-sort (filter #(<= % (first array)) (rest array)))
      [(first array)]
      (quick-sort (filter #(> % (first array)) (rest array))))))