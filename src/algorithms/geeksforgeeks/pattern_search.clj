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
(defn build-lps
  [pattern]
  (->>
  (for [i (range 0 (dec (count pattern)))
        j (range 0 i)
        :let [
              dbg (println i j)
              prefix (.substring pattern j i)
              suffix-ends-at (min (count pattern) (+ i j))
              suffix (.substring pattern (inc i) suffix-ends-at)
              ]

        :when (= prefix suffix)]
              [i (count suffix)])
  (group-by first)
  #_(map (fn [i vals] [i (map second vals)]))
  #_(map #(apply max (second %)))))
#_(build-lps "AAAA")
