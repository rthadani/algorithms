(ns algorithms.leetcode.medium)

;;2 Add two number ins a link list reversed
;;You are given two linked lists representing two non-negative numbers.
;; The digits are stored in reverse order and each of their nodes contain a single digit.
;; Add the two numbers and return it as a linked list.
;;
;;Input: (2 -> 4 -> 3) + (5 -> 6 -> 4)
;;Output: 7 -> 0 -> 8
(defn add-two-numbers
  ([list1 list2]
   (add-two-numbers (cons 0 list1) (cons 0 list2) 0 '()))
  ([list1 list2 carry soln]
   (if (and (empty? list1) (empty? list2))
     (let [r (reverse soln)]
       (if (zero? (first r)) (rest r) r))
     (let [[first1 & rest1] list1
           [first2 & rest2] list2
           sum (+ (if (nil? first1) 0 first1) (if (nil? first2) 0 first2) carry)]
       (add-two-numbers rest1 rest2 (int (/ sum 10)) (cons (mod sum 10) soln))))))

(add-two-numbers '(2 4 3) '(5 6 4))
(add-two-numbers '(2 4 3 1) '(5 6 4))

;;3. Given a string, find the length of the longest substring without repeating characters.
;;Examples:
;;Given "abcabcbb", the answer is "abc", which the length is 3.
;;Given "bbbbb", the answer is "b", with the length of 1.
;;Given "pwwkew", the answer is "wke", with the length of 3. Note that the answer must be a substring, "pwke" is a subsequence and not a substring.

(defn all-substrings
  [string]
  (let [chars (vec string)]
    (for [i (range 0 (count chars))
          j (range i (count chars))]
      (apply str (subvec chars i (inc j))))))

(defn longest-substring
  [str]
  (->> (filter #(= (count %) (count (distinct %))) (all-substrings str))
       (map count)
       (apply max)))
(longest-substring "abcabcbb")
(longest-substring "bbbbb")
(longest-substring "pwwkew")

;;5
#_(defn longest-palindromic-substring
  [string]
  (letfn [(is-palindrome? [str] (= (seq str) (reverse str)))]
    (->> (filter is-palindrome? (all-substrings string))
         (map (fn [word] [word (count word)]))
         (sort-by second)
         last
         first)))
;;Opt(i,j) = 1 if (i = j)
;;           2 + Opt(i + 1,j-1) if a[i] = a[j]
;;           max(opt(i + 1,j), opt (i, j - 1)) otherwise
(declare lps)
(defn lps*
  [s i j]
  (cond
    (= i j) [1 [(s i)]]
    (= (inc i) j) (if (= (s i) (s j)) [2 [(s i) (s j)]] [0 []])
    :else
    (let [[both pal1] (lps s (inc i) (dec j))
          [only-j pal2] (lps s (inc i) j)
          [only-i pal3] (lps s i (dec j))]
      (println pal1 pal2 pal3)
      (cond
        (= (s i) (s j)) [(+ 2 both) (conj (vec (cons (s i) pal1)) (s j))]
        (> only-j only-i) [only-j pal2]
        :else [only-i pal3]))))
(def lps (memoize lps*))

(defn longest-palindromic-substring
  [string]
  (->> (lps (vec (seq string)) 0 (dec (count string)))
       second
       (apply str)))