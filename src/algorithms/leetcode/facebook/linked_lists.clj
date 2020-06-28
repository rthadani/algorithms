(ns algorithms.leetcode.facebook.linked-lists)

(defn add-zeroes-to-smaller
  [l zeroes]
  (vec (concat l (repeat 0 zeroes))))

(defn add-two
  ([l1 l2]
   (if (= (count l1) (count l2))
     (add-two l1 l2 0)
     (if (< (count l1) (count l2))
       (add-two (add-zeroes-to-smaller l1 (- (count l2) (count l1))) l2 0)
       (add-two (add-zeroes-to-smaller l2 (- (count l1) (count l2))) l1 0)
       )))
  ([l1 l2 c]
   (if (empty? l1)
     (if (zero? c) []  [c])
     (let [n1 (first l1)
          n2 (first l2)
          sum ( + n1 n2 c)
          digit (mod sum 10)
          carry (quot sum 10)]
       (conj (add-two (rest l1) (rest l2) carry) digit)))))

(defn reorder-list
  [l])