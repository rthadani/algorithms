(ns algorithms.leetcode.google.linked-lists)

;;add two numbers
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

#_ (add-two [2  4  3] [5 6 4])

;;remove nth node
;;

(defn start-at
  [cons-cell n]
  (if (or (nil? cons-cell) (zero? n))
    cons-cell
    (recur (rest cons-cell) (dec n))))


(defn remove-nth-from-end
  ([head n]
   (remove-nth-from-end head (start-at head n) n))
  ([head start-at n]
   (cond (nil? head) head
         (nil? start-at) (rest head)
         :else (cons (first head) (remove-nth-from-end (next head) (next start-at) n)))))


#_ (remove-nth-from-end '(1 2 3 4 5) 2)

;;merge two sorted lists
(defn merge-two
  [l1 l2]
  (cond (empty? l1) l2
        (empty? l2) l1
        (< (first l1) (first l2)) (cons (first l1) (merge-two (rest l1) l2))
        :else (cons (first l2) (merge-two l1 (rest l2)))))

#_ (merge-two '(1 2 4) '(1 3 4))