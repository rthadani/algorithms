(ns algorithms.leetcode.google.arrays-strings
  (:require [clojure.string :as string]))

;;longest substring without repeats
(defn has-dupes?
  [s]
  (not= (count (into #{} s)) (count s)))

(defn longest-substring-no-repeats
  [s]
  (apply max-key count (for [i (range 0 (count s))
                             j (range i (count s))
                             :when (not (has-dupes? (subs s i j)))]
                         (subs s i j))))

(defn longest-substring-no-repeats-better
  ([s]
   (longest-substring-no-repeats-better s #{} 0))
  ([s current-set current-count]
   (cond
     (empty? s) current-count
     (current-set (first s)) (max current-count
                                  (longest-substring-no-repeats-better (rest s) (disj current-set (first s)) (dec current-count)))
     :else (longest-substring-no-repeats-better (rest s) (conj current-set (first s)) (inc current-count)))))

#_(count (longest-substring-no-repeats "abcabcbb"))
#_(longest-substring-no-repeats-better "abcabcbb")
#_(longest-substring-no-repeats-better "bbbbb")

;;container with most water
(defn container-most-water
  [wall-heights]
  (apply max
         (for [i (range 0 (count wall-heights))
               j (range (inc i) (count wall-heights))]
           (* (min (wall-heights i) (wall-heights j)) (- j i)))))

#_(container-most-water [1,8,6,2,5,4,8,3,7])


;;three sum
(defn two-sums
  [nums sum frequencies]
  (for [num nums
        :let [available (update frequencies num dec)]
        :when (and (available (- sum num)) (> (available (- sum num)) 0))]
    [num (- sum num)]))

(defn three-sums-to-zero
  [nums]
  (let [f (frequencies nums)]
    (for [num nums
          all-2-sums (two-sums nums (* num -1) (update f num dec))
          :when all-2-sums]
      (conj all-2-sums num))))

#_(three-sums-to-zero [-1, 0, 1, 2, -1, -4])


;;next-permutation
(defn find-first-non-decreasing-element-idx
  [p]
  (dec (first (reduce  (fn [[i d] c]
                         (if (<  c d)
                           (reduced [(- (count p) i) d])
                           [(inc i) c])) [0 (last p)] (reverse p)))))

(defn find-swap-index
  [p i]
  (dec (reduce (fn [r c]
                 (if (< c (p i))
                   (reduced (+ i r))
                   (inc r)))
               0
               (subvec p i))))

(defn swap-idx
  [p i j]
  (let [tmp (p i)]
    (-> (assoc p i (p j))
        (assoc j tmp))))

(defn next-permutation
  [p]
  (let [n (find-first-non-decreasing-element-idx p)
        swapped (swap-idx p n (find-swap-index p n))]
    (concat (subvec swapped 0 (inc n)) (reverse (subvec swapped (inc n))))))

#_(next-permutation [1 5 8 4 7 6 5 3 1])

(defn convert-to-digits
  [num]
  (if (empty? num)
    []
    (cons (*  (Math/pow 10 (dec (count num))) (Integer/parseInt (str (first num))))
          (convert-to-digits (rest num)))))

(defn multiply-lower
  [arr n]
  (reduce #(+ % (* %2 n)) 0 arr))

(defn multiply-strings
  [num1 num2]
  (let [n1 (convert-to-digits num1)
        n2 (convert-to-digits num2)]
    (->> n2
         (map #(multiply-lower n1 %))
         (apply +))))

#_(convert-to-digits "123")
#_(multiply-lower [100 20 3] 2)
#_(multiply-strings "123" "21")

;;rotate image
(defn transpose-matrix
  [mat]
  (apply mapv vector mat))


(defn rotate-image
  [image]
  (mapv #(vec (reverse %)) (transpose-matrix image)))

#_ (rotate-image [[1 2 3]
                     [4 5 6]
                     [7 8 9]])


;;jump-game
(defn jump-game
  ([nums]
   (jump-game nums 0))
  ([nums curr-pos]
   (println nums curr-pos)
   (if (= curr-pos (dec (count nums)))
     true
     (filter some?
             (for [i     (range 1 (nums curr-pos))
                   :when (< (+ curr-pos i) (count nums))]
               (boolean (jump-game nums (+ curr-pos i))))))))

#_ (jump-game [2,3,1,1,4])
#_ (jump-game [3 2 1 0 4])

;;plus one
(defn plus-one
  ([arr]
   (reverse (plus-one (reverse arr) 1)))
  ([arr carry]
   (if (empty? arr)
     (if (zero? carry)
       arr
       (conj arr 1))
     (let [next-carry (int (/ (+ carry (first arr)) 10))
         next-digit (int (mod (+ carry (first arr)) 10))]
       (conj (plus-one (rest arr) next-carry) next-digit)))))

#_ (plus-one [9 9 9])

;;minimum window substring
(defn has-substring? [f]
  (every? zero? (vals f)))

(defn update-frequency [f c inc-dec]
  (if (contains? f c)
    (update f c inc-dec)
    f))

#_(defn min-window-substring
  ([s t]
   (min-window-substring s t (frequencies t) 0 0))
  ([s t f l r]
   (cond (empty? s) ""
        (= r) )))

;;readn given read4

;;longest-substring with atmost 2 distinct chars
(defn longest-substring-with-2-distinct-characters
  [s]
  (apply max (for [i (range 0 (count s))
                   j (range i (count s))
                   :when (< (count (frequencies (subs s i j))) 3)]
         (- j i))))

#_ (longest-substring-with-2-distinct-characters "ccaabbb")

;;missing ranges
(defn get-range-string 
  [prev curr]
  (if (= (inc prev) (dec curr))
      (str (inc prev))
      (str (inc prev) "->" (dec curr))))

(defn missing-ranges
  [nums]
  (for [i (range 1 (inc (count nums)))
        :let [prev (nums (dec i))
            curr (when (< i (count nums)) (nums i))]
        :when (not=  (inc prev) curr)]
    (cond 
      (and (= i 1) (not (zero? prev))) (get-range-string 0 curr)
      (and (= i (count nums)) (not= 99 prev)) (get-range-string prev 100)
      :else (get-range-string prev curr))))

#_ (missing-ranges [0, 1, 3, 50, 75])

;;next-closest-time
(defn chars-in-time
  [tm]
(->>
 (frequencies tm)
 (keys)
 (remove #(= \: %))
 (into #{})))

(defn next-closest-time
  [tm]
  (let [chars (chars-in-time tm)]
    (loop [tm tm]
      (let [hours-in-minutes (* 60 (Integer/parseInt (subs tm 0 2)))
            minutes (Integer/parseInt (subs tm 3))
            curr-time-in-minutes (mod (inc (+ hours-in-minutes minutes)) (* 60 24))
            fmt-time (str (format "%02d" (inc (/ curr-time-in-minutes 60))) ":" (format "%02d" (mod curr-time-in-minutes 60)))]
        (if (= (chars-in-time fmt-time) chars)
          fmt-time
          (recur fmt-time))))))

;;expressive-words
(defn rle [word]
  (reduce
   (fn [[s rle] [c count]]
     [(str s c) (conj rle count)])
   ["" []]
   (frequencies word)))

(defn convert? [[s r1] [t r2]]
  (if (not= s t)
    false
    (= (count r2) (count (for [i     (range 0 (count r1))
                               :let  [s (r1 i)
                                      t (r2 i)]
                               :when (or (= s t) (>= t 3))]
                           1)))))

(defn expressive-words
  [s words]
  (let [rle-words (map rle words)
        rle-word (rle s)]
    (count (filter #(true? (convert? % rle-word)) rle-words))))

#_ (expressive-words "heeellooo" ["hello", "hi", "helo"])


;;
(defn can-replace-index?
  [s i m]
  (= m (subs s i (+ i (count m)))))

(defn replace-indexes
  [s indexes sources]
  (vec (for [i (range 0 (count indexes))
         :when (can-replace-index? s (indexes i) (sources i))]
     i)))

(defn find-and-replace
  [s indexes sources targets]
  (let [is (replace-indexes s indexes sources)
      x (mapv #(indexes %) is)]
    (loop [i 0
           r ""]
      (println x i (contains? x i))
      (cond 
        (>= i (count s)) r
        (not= -1 (.indexOf x i)) (recur (+ i (count (sources (.indexOf indexes i)))) (str r (targets (.indexOf indexes i))))
        :else (recur (inc i) (str r (nth s i)))))))

#_ (find-and-replace "abcd" [0,2] ["a","cd"] ["eee","ffff"])

