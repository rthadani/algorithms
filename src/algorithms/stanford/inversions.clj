(ns algorithms.stanford.inversions)

(declare merge-and-count)
(declare count*)

(defn count
  [arr]
  (second (count* arr)))

(defn- count*
  [arr]
  (if (<= (clojure.core/count arr) 1)
    [arr 0]
    (let [end (clojure.core/count arr)
          mid (int (/ end 2))
          [la lc] (count* (subvec arr 0 mid))
          [ra rc] (count* (subvec arr mid end))
          [mc ma] (merge-and-count la ra 0)]
      [ma (+ lc rc mc)])))

(defn- merge-and-count
  [la ra inv-count]
  (cond
    (empty? la) [inv-count ra]
    (empty? ra) [inv-count la]
    (< (first la) (first ra))
    (let [[c m] (merge-and-count (rest la) ra inv-count)]
      [c (cons (first la) m)])
    :else
    (let [[c m] (merge-and-count la (rest ra) inv-count)]
      [(+ c (clojure.core/count la)) (cons (first ra) m)])))

#_ (count [1 3 5 2 4 6])