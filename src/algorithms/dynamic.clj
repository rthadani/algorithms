(ns algorithms.dynamic)

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

(defn init-matrix-cost
  [n]
  (reduce #(conj % []) [] (range 0 n)))

#_(defn matrix-chain-order*
  [matrices start end]
  (if (= start end)
    0
    (min (map #(matrix-cost matrices start %) (range start (inc end))))
    (reduce #(min (matrix-chain-order )) (range start end))))
#_(defn matrix-chain-order
  [])

