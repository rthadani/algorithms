(ns algorithms.geeksforgeeks.divide-conquer)

(defn merge-count-inv
  [left right]
  (cond
   (empty? left) [0 right]
   (empty? right) [0 left]
   (> (first left) (first right))
   (let [[ci merged] (merge-count-inv left (rest right))]
     [(+ ci (count left)) (vec (cons (first right) merged))])
   :else
    (let [[ci merged] (merge-count-inv (rest left) right)]
     [ci (vec (cons (first left) merged))]) ))

(defn count-inversions
  [arr]
  (cond
   (empty? arr) [0 []]
   (= 1 (count arr)) [0 arr]
   :else
   (let [mid (int (/ (count arr) 2))
         [ci-left sl] (count-inversions (subvec arr 0 mid))
         [ci-right sr] (count-inversions (subvec arr mid))
         [ci-merge sm] (merge-count-inv sl sr)]
     [(+ ci-left ci-right ci-merge) sm])))

#_ (count-inversions [1 20 6 4 5])
