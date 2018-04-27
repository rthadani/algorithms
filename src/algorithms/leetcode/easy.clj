(ns algorithms.leetcode.easy)

;;#1
;;Given an array of integers, return indices of the two numbers such that they add up to a specific target.
(defn twosum
  [nums target]
  (first
    (for [i (range 0 (count nums))
          j (range (inc i) (count nums))
          :let [first (nth nums i)
                second (nth nums j)]
          :when (= (+ first second) target)]
      [i j])))

(twosum [2 7 11 15] 9)

;;6 Zigzag conversion
(defn make-zigzag
  [str rows]
  (last (reduce
          (fn [[row dir result] char]
            (let [r (assoc result row (conj (result row) char))
                  dir (cond
                        (zero? row) 1
                        (= row (dec rows)) -1
                        :else dir)]
              [(+ row dir) dir r]))
          [0 1 (vec (repeat rows []))]
          str)))

(defn zigzag
  [s rows]
  (->> (make-zigzag (seq s) rows)
       flatten
       (apply str)))

;;7 reverse integer
(defn assign-sign
  [i result]
  (if (> i 0)
    result
    (* -1 result)))

(defn reversed-digits
  [i]
  (loop [result 0
         curr i]
    (if (zero? curr)
      result
      (recur (+ (mod curr 10) (* result 10)) (int (/ curr 10))))))

(defn reverse-integer
  [int]
  (->> (Math/abs int)
       reversed-digits
       (assign-sign int)))


;;8 string to integer
(defn make+
  [str]
  (if (.startsWith str "-")
    (.substring str 1)
    str))

(defn atoi+
  [str]
  (reduce
    (fn [acc char] (+ (* acc 10) (- (int char) (int \0))))
    0
    str))

(defn assign-str-sign
  [orig result]
  (if (.startsWith orig "-")
    (* result -1)
    result))

(defn atoi
  [string]
  (->> (.trim string)
       make+
       atoi+
       (assign-str-sign (.trim string))))

;;9 palindrome number
(defn is-palidrome-number
  [number]
  (= number (reverse-integer number)))

;;38 Count and say
(defn to-string [say]
  (->> (partition-by identity say)
     (mapcat (fn [chars] [(count chars) (first chars)]))
     (apply str)))

(defn count-and-say
  [n]
  (if (= n 1)
    "1"
    (to-string (count-and-say (dec n)))))
