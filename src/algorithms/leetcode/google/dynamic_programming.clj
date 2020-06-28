(ns algorithms.leetcode.google.dynamic-programming)

(declare longest-palindromic-substring)
(defn longest-palindromic-substring*
  [string]
  (cond
    (empty? string) [0 ""]
    (= 1 (count string)) [1 string]
    (and (= 2 (count string)) (= (first string) (second string))) [2 string]

    :else (let [length (count string)
                [with-e e-result] (if (= (last string) (first string)) (longest-palindromic-substring (subs string 1 (dec (count string)))) [0 ""])
                [without-f f-result] (longest-palindromic-substring (subs string 1))
                [without-l l-result] (longest-palindromic-substring (subs string 0 (dec (count string))))]
            (cond (and (> with-e 0) (= (+ with-e 2) length)) [(+ 2 with-e) (str (first string) e-result (last string))]
                  (> without-f without-l) [without-f f-result]
                  :else [without-l l-result]))))

(def longest-palindromic-substring (memoize longest-palindromic-substring*))

#_ (longest-palindromic-substring "cbbd")

;;longest valid parens
(declare lvp)
(defn lvp*
  [s]
  (cond
    (<= (count s) 1) 0
    (and (= 2 (count s)) (= \( (first s)) (= \) (last s))) 2
    :else (let [with-edge (if (and (= \( (first s)) (= \) (last s))) (lvp (butlast (rest s))) 0)
                wo-first (lvp (rest s))
                wo-last (lvp (butlast s))]
            (max (if (> with-edge 0) (+ 2 with-edge) 0) wo-first wo-last))))

(def lvp (memoize lvp*))
#_ (lvp  ")()())")
#_ (lvp  "(()()()")
#_ (lvp  "(()")

;;maximum-subarray

(defn max-sum
  [a]
  (first (reduce (fn [[max s] n] [(if (> (+ s n) max) (+ s n) max) (+ s n)]) [Integer/MIN_VALUE 0] a)))

(defn cross-sum
  [left right]
  (+ (max-sum (reverse left)) (max-sum right)))

(defn max-subarray
  [a]
  (cond
    (empty? a) 0
    (= 1 (count a)) (a 0)
    :else
    (let [mid (/ (count a) 2)
          left-sum (max-subarray (subvec a 0 mid))
          right-sum (max-subarray (subvec a mid))
          c-sum (cross-sum (subvec a 0 mid) (subvec a mid))]
      (println (subvec a 0 mid) (subvec a mid) left-sum right-sum c-sum)
      (max left-sum right-sum c-sum))))

#_ (max-subarray [-2,1,-3,4,-1,2,1,-5,4])

(declare coin-change)
(defn coin-change*
  [coins amount]
  (cond
    (< amount 0) -1
    (zero? amount) 0
    :else (let [combinations (for [coin  coins
                                   :let  [res (coin-change coins (- amount coin))]
                                   :when (>= res 0)]
                               (inc res))]
            (if (empty? combinations)
              -1
              (apply min combinations)))))

(def coin-change (memoize coin-change*))

#_ (coin-change [1 2 5] 11)