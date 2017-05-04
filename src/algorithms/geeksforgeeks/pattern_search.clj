(ns algorithms.geeksforgeeks.pattern-search)

;;naive pattern search
(defn naive-search
  [pattern string]
  (for [i (range 0 (inc (- (count string) (count pattern))))
        :let [substr (.substring string i (+ i (count pattern)))]
        :when (= pattern substr)]
    i))
#_(naive-search "AABA" "AABAACAADAABAABA")

;;kmp
(defn build-t-table
  [pattern]
  (loop
    [i 0
     pos 1
     acc [0]]
    (cond
      (= pos (count pattern)) acc
      (= (nth pattern pos) (nth pattern i)) (recur (inc i) (inc pos) (conj acc (inc i)))
      :else (if (zero? i)
              (recur i (inc pos) (conj acc 0))
              (recur (nth acc (dec i)) pos acc)))))
#_(build-t-table "AAAA")
#_(build-t-table "AAACAAAAAC")

(defn knuth-morris-pratt
  [string pattern])
