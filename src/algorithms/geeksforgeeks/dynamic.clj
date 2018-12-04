(ns algorithms.geeksforgeeks.dynamic
  (:require [clojure.tools.trace :refer [trace trace-forms]]
            [clojure.string :as str]))

;;longest increasing subsequence
;;Opt(i) = 1 + max(Opt(j)) if elem(i) > elem(j) and j < i)
;;       = 1 otherwise
(declare lis)
(defn lis*
  [array]
  (if (empty? array)
    0
    (let [elem (last array)]
      (max
        (for [i (range 0 (dec (count array)))]
          (if (> elem (nth array i))
            (inc (lis (take (inc i) array)))
            0))))))

(def lis (memoize lis*))
#_(lis [3 10 2 1 20])


;;lcs longest common subseq
;;Opt(x(0,m), y(0,n)) = 1 + Opt(x(0, m - 1), y(0, n -1)) if (x(m) = y(n))
;;                      max (Opt(x(0, m - 1), y(0, n)), Opt(x(0, m), y(0, n - 1)))
(declare lcs)
(defn lcs*
  [x y]
  (cond
    (or (nil? x) (nil? y)) [0 []]
    (= (last x) (last y)) (let [[l r] (lcs (butlast x) (butlast y))]
                            [(inc l) (conj r (last x))])
    :else
    (let [[lxy-1 ry-1] (lcs x (butlast y))
          [lx-1y rx-1] (lcs (butlast x) y)]
      (if (> lxy-1 lx-1y)
        [lxy-1 ry-1]
        [lx-1y rx-1]))))

(def lcs (memoize lcs*))

#_(lcs "AGGTAB" "GXTXAYB")

;;Min cost Path
;; Find the min cost to walk through a matrix where cell is cost to pass trhough
;; minCost(m,n) = min(minCost(m-1, n-1), minCost(m, n - 1), minCost(m - 1, n)) + cost(m, n)

(declare min-cost-path)
(defn min-cost-path*
  [mat m n]
  (cond
   (and (= m 0) (= n 0)) [(get-in mat [0 0]) [[0 0]]]
   (zero? m)  (let [[c r] (min-cost-path mat m (dec n))]
                [(+ c (get-in mat [m n])) (conj r [m n])])
   (zero? n)  (let [[c r] (min-cost-path mat (dec m) n)]
                [(+ c (get-in mat [m n])) (conj r [m n])])
   :else
    (let [[costm-1n-1 arrm-1n-1] (min-cost-path mat (dec m) (dec n))
          [costmn-1 arrmn-1] (min-cost-path mat m (dec n))
          [costm-1n arrm-1n] (min-cost-path mat (dec m) n)
          costmn (get-in mat [m n])]
      (condp = (min costm-1n-1 costmn-1 costm-1n)
        costm-1n-1 [(+ costmn costm-1n-1) (conj arrm-1n-1 [m n])]
        costmn-1 [(+ costmn costmn-1) (conj arrmn-1 [m n])]
        costm-1n [(+ costmn costm-1n) (conj arrm-1n [m n])]))))
(def min-cost-path (memoize min-cost-path*))
#_(min-cost-path [[1 2 3]
                  [4 8 2]
                  [1 5 3]] 2 2)

;;r(n) = (max(0<=i<=n) (p[i] + r(n-i)))
(declare cut-rod)
(defn cut-rod*
  [p n]
  (if (zero? n)
    [0 []]
    (->>
     (for [i (range (dec n) -1 -1)
           :let [[this-cut allcuts] (cut-rod p (- n i 1))]]
       [(+ (p i) this-cut) (conj allcuts i)])
     (apply max-key first))))

(def cut-rod (memoize cut-rod*))
#_(cut-rod [1 5 8 9 10 17 17 20 24 30] 10)


;;max sum increasing subsequence
(declare msis)

#_(def msis (memoize msis*))
#_(msis [1 101 2 3 100 4 5])

;;justify-text
; jt(i) = arg-min(badness[i, j] + jt(j)) where i<j<n
;       = 0 if i = n

(defn line-length
  [words]
  (if (empty? words)
    0
    (reduce (fn [c w] (+ c (count w)))
            (dec (count words))
            words)))

(defn badness
  [words width]
  (let [l (line-length words)]
    (if (> l width)
      Integer/MAX_VALUE
      (Math/pow (- width l) 2))))

(defn arg-min
  [array]
  (println "argmin" array)
  (if (empty? array)
    0
    (->> (map vector (range 0 (count array)) array)
         (apply min-key second)
         first)))

(declare justify-text)
(defn justify-text*
  [words width]
  (if (empty? words)
   0
   (let [b (for [i (range 0 (count words))
                 :let [_ (println (subvec words i))]
                 ]
            (+ (badness  (subvec words i) width)
               (justify-text (subvec words 0 i) width)))]
     (if (empty? b)
       0
       (apply min b)))))

(def justify-text (memoize justify-text*))
#_(justify-text (str/split "tushar roy likes to code" #" ") 10)

