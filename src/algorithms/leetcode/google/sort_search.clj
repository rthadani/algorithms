(ns algorithms.leetcode.google.sort-search)

;;median of sorted arrays

;;find first and last in sorted
(defn binary-search-index
  [nums l r t]
  (if (> l r)
    -1
    (let [mid (+ l (int (/ (- r l) 2)))]
      (cond
        (= (nums mid) t)  mid
        (< t (nums mid)) (binary-search-index nums l (dec mid) t)
        :else (binary-search-index nums (inc mid) r t)))))
#_ (binary-search-index [1 2 3 4 5 6] 0 5 2)
(defn search-range
  [nums target]
  (let [idx (binary-search-index nums 0 (dec (count nums)) target)]
    (println idx)
    (if (= idx -1) [-1 -1]
        (reduce (fn [[l r] i] [(if (and (>= (dec l) 0) (= target (nums (dec l)))) (dec l) l)
                               (if (and (< (inc r) (count nums)) (= target (nums (inc r)))) (inc r) r)])
                [idx idx]
                (range 0 (count nums))))))

#_ (search-range [5,7,7,8,8,10] 8 )
;;merge-intervals
(defn merge-intervals
  [intervals]
  (loop [r []
         i intervals]
    (if (empty? i) 
      r
      (let [next-start    (ffirst i)
            next-end      (second (first i))
            current-start (when (seq r) (first (last r)))
            current-end   (when (seq r) (second (last r)))]
        (cond 
          (empty? r) (recur (conj r (first intervals)) (rest intervals))
          (< next-start current-end) (recur (assoc r (dec (count r)) [(min current-start next-start) (max next-end current-start)]) (rest i))
          :else (recur (conj r [next-start next-end]) (rest i)))))))

#_ (merge-intervals [[1,3],[2,6],[8,10],[15,18]])

;;insert interval
;;

(defn insert-interval
  [given-intervals interval-to-add]
  (->> given-intervals
       (reduce
        (fn [[merged result] current]
          (let [[ms me] merged
                [cs ce] current]
            (cond
              (empty? merged)  [[] (conj result current)]
              (< ce ms)  [merged (conj result current)]
              (or (< cs ms ce) (< ms cs me)) [[(min ms cs) (max me ce)] result]
              (> cs me) [[] (conj result merged current)])))
        [interval-to-add []])
       second))

#_ (insert-interval [[-4 -1] [0 2] [3 6] [7 9] [11 12] [14 17]] [1 8])

(defn valid-anagram
  [s t]
  (let [fs (frequencies s)
       ft (frequencies t)]
    (every? (fn [[k c]] (and (contains? ft k) (= (ft k) c))) fs)))

#_ (valid-anagram "anagram" "nagaram")
#_ (valid-anagram "rat" "car")

;;count of smaller numbers
;;count inversions but keep indexes that are being swapped not jsut counts