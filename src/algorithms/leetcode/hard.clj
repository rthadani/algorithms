(ns algorithms.leetcode.hard
    (:require [clojure.core.match :refer [match]]
              [clojure.data.priority-map :refer [priority-map-by]]))

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
          x (nth arr mid)]
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
        (median (subvec a (inc idx1) (count a)) (subvec b 0 idx2))
        (median (subvec a 0 idx1) (subvec b (inc idx2) (count b)))))))

#_(median [1 2 3 4 5] [6 7 8])
#_(median [6 7] [1 2 3 4 5])
#_(median [1 3] [2 4])


(defn is-match
  [string pattern]
(println string pattern )
  (cond
    (empty? pattern) (empty? string)
    (= 1 (count pattern)) (and (not (empty? string)) (and (or (= \. (first pattern)) (= (first pattern) (first string)))
                                                          (is-match (rest string) (rest pattern))))
    (not= (second pattern) \*) (and (or (= (first pattern) \.) (= (first pattern) (first string)))
                                    (is-match (rest string) (rest pattern)))
    
    :else
    (or (is-match string (rest (rest pattern)))
        (loop [c string]
          (cond
            (empty? c) false
            (or (= \. (first pattern)) (= (first pattern) (first c)))
            (or (is-match (rest c) (rest (rest pattern))) (recur (rest c)))
            :else (recur (rest c)))))))

#_ (is-match "aab" "a.*b")



(defn- split-buildings
  [buildings]
  (->> buildings
       (mapcat (fn [[s e h]] [{:x s :h h :s true} {:x e :h h :s false}]))
       (sort (fn [x y] (if-not (= (:x x) (:x y)) 
                         (- (:x x) (:x y))
                         (if (:s x) 1 -1))))))

(defn- process-next-entry
  [{:keys [x h s]} result heap]
  (let [[_ max-height] (first heap)]
    (if s
      (if (> h max-height)
        [(conj result [x h]) (assoc heap h h)]
        [result (assoc heap h h)])
      (let [new-heap (dissoc heap h)
            [_ current-top] (first new-heap)]
        (if (not= current-top max-height)
          [(conj result [x current-top]) new-heap]
          [result new-heap])))))

(defn- sky-line*
  [buildings]
  (->> buildings
       split-buildings
       (reduce
        (fn [[result heap] entry]
          (process-next-entry entry result heap))
        [[] (priority-map-by > 0 0)])))

(defn sky-line
  [buildings]
  (first (sky-line* buildings)))

#_ (sky-line [ [2 9 10], [3 7 15], [5 12 12], [15 20 10], [19 24 8]])