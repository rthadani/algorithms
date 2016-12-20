(ns algorithms.leetcode.hard
  (:require [clojure.core.match :refer [match]]))

;;There are two sorted arrays nums1 and nums2 of size m and n respectively.
;; Find the median of the two sorted arrays. The overall run time complexity should be O(log (m+n)).

(defn mid-index
  [a]
  (int (/ (count a) 2)))

(defn insert
  [e arr]
  (if (= 1 (count arr))
    (if (< e (first arr))
      [e (first arr)]
      [(first arr) e])
    (let [mid (mid-index arr)
        x (nth arr mid)  ]
      (if (< e x)
        (vec (concat (insert e (subvec arr 0 mid)) (subvec arr mid (count arr))))
        (vec (concat (subvec arr 0 mid) (insert e (subvec arr mid (count arr)))))))))


(defn get-median
  [arr]
  (let [mid (mid-index arr)]
    (if (even? (count arr))
      (/ (+ (nth arr mid) (nth arr (dec mid))) 2)
      (nth arr mid))))

(defn median
  [a b]
  (println a b)
  (cond
    (empty? a) (get-median b)
    (empty? b) (get-median a)
    (= 1 (count a)) (get-median (insert (first a) b))
    (= 1 (count b)) (get-median (insert (first b) a))
    :else
    (let [idx1 (mid-index a)
          x (nth a idx1)
          idx2 (mid-index b)
          y (nth b idx2)]
      (if (< x y)
        (median (subvec a (inc idx1)(count a)) (subvec b 0 idx2))
        (median (subvec a 0 idx1) (subvec b (inc idx2) (count b)))))))

#_(median [1 2 3 4 5] [6 7 8])
#_(median  [6 7] [1 2 3 4 5] )
#_(median [1 3 ] [2 4])
#_(media)
