(ns algorithms.epi.primitive)

(defn parity
  [^Long d]
  (loop [result 0
         d d]
    (if (zero? d)
      result
      (recur (bit-xor result (bit-and d 1))
             (bit-shift-right d 1)))))
#_(parity 8)
#_(parity 9)

(defn parity-2
  [^Long d]
  (loop [result 0
         d d]
    (if (zero? d)
      result
      (recur (bit-xor result 1)
             (bit-and d (dec d))))))
#_(parity-2 8)
#_(parity-2 9)

(defn reverse-digits
  [n]
  (if (< n 0)
    (* -1 (reverse-digits (* -1 n)))
    (loop [result 0
           n n]
      (if (zero? n)
        result
        (recur (+ (* result 10) (mod n 10))
               (int (/ n 10)))))))

#_(reverse-digits 123)
#_(reverse-digits -123)

(defn pow
  [^Double x ^Double n]
  (cond
    (< n 0) (pow (/ 1 x) (* -1 n))
    (zero? n) 1
    (even? n) (let [pow-n2 (pow x (/ n 2))] (* pow-n2 pow-n2))
    :else (* x (pow x (dec n)))))

#_(pow 2 4)
#_(pow 2 3)
#_(pow 2 -3)

(defn palindrome-number
  [n]
  (println n)
  (if (zero? n)
    true
    (let [tens-places (int (Math/floor (Math/log10 n)))
          msd-mask (pow 10 tens-places)
          ms-digit (int (/ n msd-mask))
          ls-digit (mod n 10)]
      (and (= ms-digit ls-digit)
           (palindrome-number (int (/ (mod n msd-mask) 10)))))))

#_ (palindrome-number 1231)