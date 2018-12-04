(ns algorithms.epi.heap
    (:require [algorithms.heaps :refer :all]))

(defn- add-to-result-from-heap
  [heap result comparator]
  [(delete-min-binary-heap heap comparator)
   (conj result (first (find-min-binary-heap heap)))
   (second (find-min-binary-heap heap))])

(defn merge-sorted-arrays
  ([arrays]
   (letfn [(min-heap-comparator [x y] (< (first x) (first y)))]
     (-> 
       (map-indexed (fn [idx arr] (mapv (fn [a] [a idx]) arr)) arrays)
       (merge-sorted-arrays min-heap-comparator))))
  ([arrays min-heap-comparator]
   (loop [heap (reduce (fn [heap array] (insert-binary-heap heap (first array) min-heap-comparator)) [] arrays)
          arrays (mapv (fn [array] (into [] (rest array))) arrays)
          result []]
     (if (empty? heap)
       result
       (let [[heap-without-element result min-element-array-index] (add-to-result-from-heap heap result min-heap-comparator)
             min-element-array (get arrays min-element-array-index)
             new-element-array (into [] (rest min-element-array))
             new-min-element (first min-element-array)
             new-heap (if (empty? min-element-array) heap-without-element (insert-binary-heap heap-without-element new-min-element min-heap-comparator))
             new-arrays (assoc arrays min-element-array-index new-element-array)]
         (recur new-heap new-arrays result))))))

(merge-sorted-arrays [[0 4 42] [1 2] [3  5 35]])

(defn- break-into-sublists
  [arr]
  (last 
   (reduce
    (fn [[state current result] i]
      (cond 
        (= 0 i) [state (conj current (get arr i)) result]
        (and (= current :increasing) (< (get arr i) (last current))) [:decreasing [(get arr i)] (conj result current)]
        (and (= current :decreasing) (> (get arr i) (last current))) [:increasing [(get arr i)] (conj result (into [] (reverse current)))]
        :else [state (conj current (get arr i)) result]))
    [:increasing [] []]
    (range 0 (count arr)))))
(defn sort-increasing-decreasing
 [array]
 (-> break-into-sublists array
    merge-sorted-arrays))

(defn sort-mostly-sorted
  [arr k]
  (loop [heap (reduce  
               (fn [heap i] (insert-binary-heap heap (get arr i) min-heap-lighter)) 
               [] 
               (range 0 (max k (count arr))))
         array (subvec arr (max k (count arr)))
         result []]
        (if (empty? heap)
          result
          (let [heap-without-top (delete-min-binary-heap heap min-heap-lighter)
                new-heap (if (empty? array) heap-without-top (insert-binary-heap heap-without-top (first array) min-heap-lighter))]
            (recur new-heap (into [] (rest array)) (conj result (find-min-binary-heap heap)))))))

(sort-mostly-sorted [3 -1 2 6 4 5 8] 2)

(declare distance-comparator)
(declare add-new-element-to-heap)
(defn k-nearest-stars
  [star-points k]
  (let [heap (reduce (fn [heap star] (add-new-element-to-heap heap star distance-comparator k)) [] star-points)]
    (loop [heap heap
           result []]
          (if (empty? heap)
            result
            (recur (delete-min-binary-heap heap distance-comparator) 
                   (conj result (find-min-binary-heap heap)))))))
(defn- distance-comparator
  [star1 star2] 
  (letfn [(distance [s] (Math/sqrt (apply + (map #(Math/pow % 2) s))))]
         (< (distance star1) (distance star2))))

(defn- add-new-element-to-heap
  [heap element comparator size]
  (if (> (count heap) size)
    (-> (insert-binary-heap heap element comparator)
        (delete-min-binary-heap comparator))
    (insert-binary-heap heap element comparator)))



(defn online-median
  [seq]
  (loop [min-heap []
         max-heap [(first seq)]
         seq (rest seq)]
        (if (empty? seq)
          (if (= (count min-heap) (count max-heap))
            (/ (+ (find-min-binary-heap min-heap) (find-min-binary-heap max-heap)) 2)
            (find-min-binary-heap min-heap))
          (let [max-heap-with-next-element (insert-binary-heap max-heap (first seq) max-heap-lighter)
                [new-min-heap new-max-heap]
                (if (> (- (count max-heap-with-next-element) (count min-heap)) 1)
                  [(insert-binary-heap min-heap (find-min-binary-heap max-heap-with-next-element))
                   (delete-min-binary-heap max-heap-with-next-element)]
                  [min-heap max-heap-with-next-element])]
            (recur new-min-heap new-max-heap (rest seq)))))) 
