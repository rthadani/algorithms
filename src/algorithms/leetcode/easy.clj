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

