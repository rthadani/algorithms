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

;;weighted job indexes
;;opt(j) = max(opt(j-1), v(n) + opt(p(j)))
;where p(j) = the righttmost interval before j that does not overlap with j -1 if no interval found
(defn make-job-indexes
  [jobs]
  (let [sorted-jobs (sort-by second jobs)
        indexed (into {}
                      (for [i (range 0 (count sorted-jobs))
                            j (range 0 (count sorted-jobs))
                            :let [[si fi _] (nth sorted-jobs i)
                                  [sj fj _] (nth sorted-jobs j)]
                            :when (<= fj si)]
                        [i j]))
        missing-keys (set/difference (into #{} (range 0 (count sorted-jobs))) (keys indexed))
        indexed-missing-keys (into {} (map (fn [key] [key -1]) missing-keys))]
    [sorted-jobs (merge indexed indexed-missing-keys {-1 -1})]))

(declare wis)

(defn wis*
  [jobs job-indexes j soln]
  (if (= -1 j)
    0
    (let [[_ _ v] (nth jobs j)
          value-j (max (+ v (wis jobs job-indexes (get job-indexes j) soln))
                       (wis jobs job-indexes (dec j) soln))]
      (swap! soln assoc j value-j)
      value-j)))

(def wis (memoize wis*))

(defn print-solution-wis
  [j jobs job-indexes soln]
  (when (not= j -1)
    (let [[_ _ v] (nth jobs j)
        valuepj (if (= -1 (get job-indexes j)) 0 (get soln (get job-indexes j)))
        valuej-1 (if (< (dec j) 0) 0 (get soln (dec j)))   ]
      (if (> (+ v valuepj)  valuej-1)
        (do
          (println j)
          (print-solution-wis (get job-indexes j) jobs job-indexes soln))
        (print-solution-wis (dec j) jobs job-indexes soln)))))

(defn weighted-interval-schedule
  [jobs]
  (let [[sorted-jobs pjs] (make-job-indexes jobs)
        soln (atom (vec (range 0 (count jobs))))]
    (wis sorted-jobs pjs (dec (count sorted-jobs)) soln)
    (println soln)))




#_(weighted-interval-schedule [[1 2 50]
                               [3 5 20]
                               [6 19 100]
                 amaz              [2 100 200]])

#_(weighted-interval-schedule-with-soln [[1 2 50]
                                         [3 5 20]
                                         [6 19 100]
                                         [2 100 200]])

;;Max Subset sum
;If w < wi then opt(i, w) = opt(i - 1 ,w)
; else Opt(i,w) = max (Opt(i-i, w), w(i) + Opt(i - 1, w - wi))
(declare subset-sum)
(defn subset-sum*
  [i weights w]
  (cond
    (< i 0) [0 []]
    (<= w 0) [0 []]
    :else
    (let [wi (weights i)]
      (if (> wi w)
        (subset-sum (dec i) weights w)
        (let [[v-withouti items-witouti :as without-i] (subset-sum (dec i) weights w)
              [v-with-i items-with-i as :as with-i] (subset-sum (dec i) weights (- w wi))]
          (if (> (+ v-with-i wi) v-withouti)
            [(+ v-with-i wi) (conj items-with-i i)]
            without-i ))))))

(def subset-sum (memoize subset-sum*))
#_(def soln (atom (vec (range 0 6))))
#_(subset-sum 5 [3 34 4 12 5 2] 7)
