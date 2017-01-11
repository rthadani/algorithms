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
  [j jobs pj]
  (if (< j 0)
    [0 []]
    (let [[_ _ v] (nth jobs j)
          [v+j pjs :as with-j] (wis (get pj j) jobs pj)
          [v-j _ :as without-j] (wis (dec j) jobs pj)]
      (if (> (+ v v+j) v-j)
        [(+ v v+j) (conj pjs j)]
        without-j))))
(def wis (memoize wis*))

(defn weighted-interval-schedule
  [jobs]
  (let [[sorted-jobs pjs] (make-job-indexes jobs)
        soln (atom (vec (range 0 (count jobs))))]
    (wis (dec (count sorted-jobs)) sorted-jobs pjs)))

#_(weighted-interval-schedule [[1 2 50]
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
    (= w 0) [0 []]
    :else
    (let [wi (weights i)]
      (if (> wi w)
        (subset-sum (dec i) weights w)
        (let [[v-i items-i :as without-i] (subset-sum (dec i) weights w)
              [v+i items+i as :as with-i] (subset-sum (dec i) weights (- w wi))]
          (if (> (+ v+i wi) v-i)
            [(+ v+i wi) (conj items+i i)]
            without-i))))))

(def subset-sum (memoize subset-sum*))
#_(subset-sum 5 [3 34 4 12 5 2] 13)
;;for knapsack replace w with v the value of an item when comparing

;;RNA secondary structure
;Opt(i,j) = max(Opt(i, j - 1), max(1 + Opt(i, t - 1)  + Opt(t + 1, j - 1))
; where t is atleast 4 units away from j
(declare rna-struct)
(defn rna-struct*
  [rna i j]
  (println i j)
  (cond
    (< i 0) 0
    (< (- j i) 4) 0
    (and (= \A (nth rna i)) (not= \U (nth rna j))) 0
    (and (= \U (nth rna i)) (not= \A (nth rna j))) 0
    (and (= \C (nth rna i)) (not= \G (nth rna j))) 0
    (and (= \G (nth rna i)) (not= \C (nth rna j))) 0
    :else
    (max (rna-struct rna i (dec j)) (+ 1 (rna-struct rna i (- j 3)) (rna-struct rna (inc (- j 3)) (dec j))))))
(def rna-struct (memoize rna-struct*))

#_(def rna (seq "ACCGGUAGU"))
#_(rna-struct* rna 0 (dec (count rna)))

;;Sequence alignment
;;opt (i,j) = min(alpha(xi,yi) + Opt(i - 1, j - 1), delta + Opt(i - 1, j), delta + Opt(i, j-1))
; where alpha is the penalty when letters from both sequences are used
;delta is the penalty when a blank is used
(declare sequence-alignment)
(defn sequence-alignment*
  [x y alpha delta]
  #_(println x y)
  (match [(count x) (count y)]
         [0 0] [0 [] []]
         [0 _] [(delta (count y)) (vec (repeat (count y) \_)) y]
         [_ 0] [(delta (count x)) x (vec (repeat (count x) \_))]
         [_ _] (let [lx (last x)
                     ly (last y)
                     a (alpha lx ly)
                     [ca vec1x vec1y] (sequence-alignment (vec (butlast x)) (vec (butlast y)) alpha delta)
                     [cx vec2x vec2y] (sequence-alignment (vec (butlast x)) y alpha delta)
                     [cy vec3x vec3y] (sequence-alignment x (vec (butlast y)) alpha delta)]
                 (condp = (min (+ a ca) (+ (delta 1) cx) (+ (delta 1) cy))
                   (+ a ca) [(+ a ca) (conj vec1x lx) (conj vec1y ly)]
                   (+ (delta 1) cx) [(+ (delta 1) cx) (conj vec2x lx) (conj vec2y \_)]
                   (+ (delta 1) cy) [(+ (delta 1) cy) (conj vec3x \_) (conj vec3y ly)]))) )

(defn this-delta
  [how-many]
  (* 2 how-many))

(defn this-alpha
  [xi yi]
  (let [vowels #{\a \e \i \o \u}
        consonants #{\b \c \d \f \g \h \j \k \l \m \n \p \q \r \s \t \u \v \w \x \y \z}]
    (cond
      (= xi yi) 0
      (and (vowels xi) (vowels yi)) 1
      (and (consonants xi) (consonants yi)) 1
      (and (vowels xi) (consonants yi)) 3
      (and (consonants xi) (vowels yi)) 3)))

(def sequence-alignment (memoize sequence-alignment*))

#_(sequence-alignment
    (vec (seq "mean"))
    (vec (seq "name"))
    this-alpha
    this-delta)

