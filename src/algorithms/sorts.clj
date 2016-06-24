(ns algorithms.sorts
  (:require [clojure.core.match :refer [match]]
            [algorithms.heaps :refer :all]))

(defn quick-sort
  [array]
  (if-not (seq array)
    []
    (concat
      (quick-sort (filter #(<= % (first array)) (rest array)))
      [(first array)]
      (quick-sort (filter #(> % (first array)) (rest array))))))

(defn- insert-in-sorted-array
  [element result]
  (cond
    (empty? result) [element]
    (< element (first result)) (cons element result)
    :else (cons (first result) (insert-in-sorted-array element (rest result)))))

(defn insertion-sort
  [array]
  (if (empty? array)
    array
    (let [element (first array)
          sorted (insertion-sort (rest array))]
      (insert-in-sorted-array element sorted))))

(defn heap-sort
  [array]
  (let [heap (build-max-heap array)]
    (loop [heap heap
           result []]
      (println "****" heap)
      (if (empty? heap)
        result
        (recur (max-heapify (vec (rest heap)) 0)
               (cons (first heap) result))))))

(defn merge-arrays
  [left right]
  (cond
    (and (empty? left) (empty? right)) []
    (empty? left) right
    (empty? right) left
    (< (first left) (first right)) (cons (first left) (merge-arrays (rest left) right))
    :else (cons (first right) (merge-arrays left (rest right)))))

(defn merge-sort*
  [array l r]
  (if (>= l r)
    [(array l)]
    (let [mid (int (Math/floor (/ (+ l r) 2)))]
      (merge-arrays (merge-sort* array l mid) (merge-sort* array (inc mid) r)))))

(defn merge-sort
  [array]
  (merge-sort* array 0 (dec (count array))))

(def to-sort (shuffle (range 0 19)))
