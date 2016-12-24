(ns algorithms.dynamic
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]))

(declare cut-rod)

(defn cut-rod* [p n]
  (if (zero? n)
    0
    (loop [i 1
           max-value Integer/MIN_VALUE]
      (if (not= i n)
        (recur (inc i) (max max-value (+ (p (dec i)) (cut-rod p (- n i)))))
        (max max-value (p (dec i)))))))

(def memoized-cut-rod
  (memoize cut-rod*))

(defn cut-rod
  [p n]
  (memoized-cut-rod p n))


(def rod-prices
  [1 5 8 9 10 17 17 20 24 30])

(defn fibonacci
  [n]
  (match [n]
         [0] 0
         [1] 1
         [_] (+ (fibonacci (dec n)) (fibonacci (- n 2)))))

(defn td-fib
  [n]
  (with-redefs [fibonacci (memoize fibonacci)]
    (fibonacci n)))

(defn bottom-up-fib
  [n]
  (->
    (iterate (fn [[x y]] [y (+ x y)]) [0 1])
    (nth n)
    first))

(declare levenshtein)

(defn levenshtein*
  [x y]
  (match [(count x) (count y)]
         [0 _] (count y)
         [_ 0] (count x)
         [_ _] (min (-> (levenshtein (butlast x) y)
                        (inc))
                    (-> (levenshtein x (butlast y))
                        (inc))
                    (+ (levenshtein (butlast x) (butlast y))
                       (if (= (last x) (last y)) 0 1)))))

(def levenshtein
  (clojure.core/memoize levenshtein*))

;(levenshtein "sweep" "sleep")

;;weighted interval scheduling
(defn sort-by-finish-times
  [jobs]
  (sort-by #(second %) jobs))

(defn make-job-indexes
  [jobs]
  (let [sorted-jobs (sort-by second jobs)
        indexed (into {}
                      (for [i (range 0 (count sorted-jobs))
                            j (range 0 (count sorted-jobs))
                            :let [[si fi _] (nth sorted-jobs i)
                                  [sj fj _] (nth sorted-jobs j)]
                            :when (< fj si)]
                        [i j]))
        missing-keys (set/difference (into #{} (range 0 (count sorted-jobs))) (keys indexed))
        indexed-missing-keys (into {} (map (fn [key] [key -1]) missing-keys))]
    [sorted-jobs (merge indexed indexed-missing-keys {-1 0})]))

(declare wis)
(defn wis*
  [jobs job-indexes j]
  (if (= -1 j)
    0
    (let [[_ _ v] (nth jobs j)]
     (max (+ v (wis jobs job-indexes (get job-indexes j)))
          (wis jobs job-indexes (dec j))))))

(def wis (memoize wis*))

(defn weighted-interval-schedule
  [jobs]
  (let [ [sorted-jobs pjs](make-job-indexes jobs)]
    (wis sorted-jobs pjs (dec (count sorted-jobs)))))

