(ns algorithms.geeksforgeeks.dynamic
  (:require [clojure.tools.trace :refer [trace trace-forms]]))

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
   (let [[lxy-1 ry-1] (lcs  x (butlast y))
         [lx-1y rx-1] (lcs  (butlast x) y) ]
     (if (> lxy-1 lx-1y)
       [lxy-1 ry-1]
       [lx-1y rx-1]))))

(def lcs (memoize lcs*))

#_ (lcs "AGGTAB" "GXTXAYB")

;;Min cost Path
;; Find the min cost to walk through a matrix where cell is cost to pass trhough
;; minCost(m,n) = min(minCost(m-1, n-1), minCost(m, n - 1), minCost(m - 1, n)) + cost(m, n)

(declare min-cost-path)
(defn min-cost-path*
  [mat m n]
  (if (or  (= m 0) (= n 0))
    [(+ (get-in mat [0 0]) (get-in mat [m n]))  [[0 0] [m n]]]
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
    0
    (->>
     (for [i (range (dec n) -1 -1)]
       (+  (p i) (cut-rod p (- n i 1))))
     (apply max))))
(def cut-rod (memoize cut-rod*))
#_(cut-rod [1 5 8 9 10 17 17 20 24 30] 4)


;;max sum increasing subsequence
(declare msis)

#_(def msis (memoize msis*))
#_ (msis [1 101 2 3 100 4 5])
