(ns algorithms.epi.search)

(defn binary-search
  ([arr num]
   (binary-search arr num 0 (dec (count arr))))
  ([arr num start end]
   (if (> start end)
     -1
     (let [mid (int (+ start (/ (- end start) 2)))]
          (cond
            (= num (arr mid)) mid
            (< num (arr mid)) (binary-search arr num start mid)
            :else (binary-search arr num (inc mid) end))))))

#_ (binary-search [1 2 3 4 5] 3)
#_ (binary-search [1 2 3 4 5 6] 3)
#_ (binary-search [1 2 3 4 5 6] 7)
#_ (binary-search [] 3)

(defn first-occurence-search
  ([arr num]
   (first-occurence-search arr num 0 (dec (count arr)) -1))
  ([arr num start end first-found-at]
   (if (> start end)
     first-found-at
     (let [mid (int (+ start (/ (- end start) 2)))]
       (cond
         (= num (arr mid)) (first-occurence-search arr num start (dec mid) mid)
         (< num (arr mid)) (binary-search arr num start mid)
         :else (binary-search arr num (inc mid) end))))))


#_ (first-occurence-search [1 2 2 3 4 5] 3)
#_ (first-occurence-search [1 2 2 3 4 5] 2)
#_ (first-occurence-search [1 2 2 3 4 5 5] 5)


(defn smallest-element-shifted-array
  ([arr]
   (smallest-element-shifted-array arr  0 (dec (count arr))))
  ([arr start end]
   (println start end)
   (let [mid (int (+ start (/ (- end start) 2)))]
     (cond
       (== start end) start
       (> (arr mid) (arr end)) (smallest-element-shifted-array arr (inc mid) end)
       :else (smallest-element-shifted-array arr start mid)))))

(smallest-element-shifted-array [369 378 478 550 631 103 203 220 234 279 368])

(defn integer-square-root
  ([k]
   (integer-square-root 0 k k))
  ([start end k]
(println start end k)
   (let [mid (int (/ (+ start end) 2))]
     (cond 
       (== start end) (dec start)
       (> (* mid mid) k) (integer-square-root start (dec mid) k)
       :else (integer-square-root (inc mid) end k)))))

(integer-square-root 300)

(defn search-2d-sorted-array
  ([a num]
   (search-2d-sorted-array a num (dec (count a)) (dec (count (a 0)))))
([a num row col]
 (cond 
   (or (< col 0) (>= row (count a))) false
   (= num (get-in a [row col])) true
   (> num (get-in a [row col])) (search-2d-sorted-array a num (inc row) col)
   :els (search-2d-sorted-array a num row (dec col)) )))