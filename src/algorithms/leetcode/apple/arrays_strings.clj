(ns algorithms.leetcode.apple.arrays-strings)

(defn two-sum
  [a t]
  (second (reduce  (fn [[seen r] [i e]] 
                     (if (contains? seen (- t e))
                       (reduced [seen [(get seen (- t e)) i]])
                       [(assoc seen e i) r]))
                   [{} []]
                   (map-indexed (fn [i e] [i e]) a))))

#_ (two-sum [2,7,11,15] 9)
#_ (two-sum [3 3] 6)
#_ (two-sum [3] 6)
(defn two-sum-sorted
  [n i]
  (loop [low (inc i)
         high (dec (count n))
         r []]
    (if 
     (>= low high) 
      r
     (let [sum (+ (n i) (n low) (n high))]
       (cond 
         (< sum 0) (recur (inc low) high r)
         (> sum 0) (recur low (dec high) r)
         :else (recur (inc low) (dec high) (conj r [(n i) (n low) (n high)])))))))

(defn three-sum-zero
  [n]
  (first (filter not-empty (let [sorted (vec (sort n))]
                             (for [i (range 0 (count sorted))
                                   :when (or (zero? i) (and (<= (sorted i) 0) (not= (sorted (dec i)) (sorted i))))]
                               (two-sum-sorted sorted i))))))

#_ (three-sum-zero [-1,0,1,2,-1,-4])

;;String to integer
(defn my-atoi
  ([s] (my-atoi s 0 false 1))
  ([s result sign-seen sign]
   (cond
     (empty? s) (* sign result)
     (and (= \space (first s)) (zero? result) (not sign-seen)) (my-atoi (rest s) result sign-seen sign)
     (and (or (= \+ (first s)) (= \- (first s))) (zero? result) (not sign-seen)) (my-atoi (rest s) result true (if (= \+ (first s)) 1 -1))
     (and (>= (int (first s)) 48) (<= (int (first s)) 57)) (my-atoi (rest s) (+ (* 10 result) (- (int (first s)) 48)) sign-seen sign)
     :else (* sign result))))

#_ (my-atoi "42")
#_ (my-atoi "      -42")
#_ (my-atoi "-4123 with words")

(def syms {1 \I
           5 \V
           10 \X
           100 \C
           50 \L
           500 \D
           1000 \M})
(defn integer-roman
  [i]
  (cond
    (zero? i) ""
    (>= i 1000) (str (syms 1000) (integer-roman (- i 1000)))
    (>= i 900) (str (syms 100) (syms 1000) (integer-roman (- i 1000)))
    (>= i 500) (str (syms 500) (integer-roman (- i 500)))
    (>= i 400) (str (syms 100) (syms 500) (integer-roman (- i 400)))
    (>= i 100) (str (syms 100)  (integer-roman (- i 100)))
    (>= i 90) (str (syms 10)(syms 100)  (integer-roman (- i 90)))
    (>= i 50) (str (syms 50)  (integer-roman (- i 50)))
    (>= i 40) (str (syms 10)(syms 50)  (integer-roman (- i 40)))
    (>= i 10) (str (syms 10) (integer-roman (- i 10)))
    (>= i 9) (str (syms 1)(syms 10) (integer-roman (- i 9)))
    (>= i 5) (str (syms 5) (integer-roman (- i 5)))
    (>= i 4) (str (syms 1)(syms 5) (integer-roman (- i 4)))
    (>= i 1) (str (syms 1) (integer-roman (- i 1)))
    ))
#_ (integer-roman 3888)