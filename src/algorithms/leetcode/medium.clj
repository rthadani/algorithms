(ns algorithms.leetcode.medium
  (:require [clojure.data.priority-map :refer [priority-map-by]]))

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

;;11 container with most water


;;12 integer to roman
(def conversion-map
  (sorted-map 1 "I"
              2 "II"
              3 "III"
              4 "IV"
              5 "V"
              6 "VI"
              7 "VII"
              8 "VIII"
              9 "IX"
              10 "X"
              14 "XIV"
              15 "XV"
              19 "XIX"
              20 "XX"
              30 "XXX"
              40 "XL"
              50 "L"
              60 "LX"
              70 "LXX"
              80 "LXXX"
              90 "XC"
              100 "C"
              200 "CC"
              300 "CCC"
              400 "CD"
              500 "D"
              600 "DC"
              700 "DCC"
              800 "DCCC"
              900 "CM"
              1000 "M"))

(defn find-largest-match
  [i integers]
  (if (= (count integers) 1)
    (first integers)
    (let [mid (int (/ (count integers) 2))]
      (if (>= i (integers mid))
        (recur i (subvec integers mid))
        (recur i (subvec integers 0 mid))))))

(defn convert-to-roman
  [i]
  (if (zero? i)
    ""
    (let [largest-key (find-largest-match i (vec (keys conversion-map)))]
      (str (get conversion-map largest-key) (convert-to-roman (- i largest-key))))))
#_(convert-to-roman 1)
#_(convert-to-roman 6)
#_(convert-to-roman 12)
#_(convert-to-roman 99)
#_(convert-to-roman 3999)

;;13 Roman to integer
(def digit-map
  {\I 1
   \V 5
   \X 10
   \L 50
   \C 100
   \D 500
   \M 1000})

(defn roman-to-decimal
  ([roman] (roman-to-decimal (reverse roman) 0))
  ([roman current-dec]
   (if (empty? roman)
     0
     (let [digit (get digit-map (first roman))
           multiplier (if (< digit current-dec) -1 1)]
       (+ (* digit multiplier) (roman-to-decimal (rest roman) digit))))))
#_(roman-to-decimal "MCMXCIX")


;;14 Longest compn prefix
(defn longest-common-prefix
  [strings]
  (let [sorted (sort-by #(count %) strings)
        shortest (first sorted)
        rest (rest sorted)]
    (loop [shortest shortest]
      (if (or (zero? (count shortest))
              (every? #(.startsWith % shortest) rest))
        shortest
        (recur (.substring shortest 0 (dec (count shortest))))))))
#_(longest-common-prefix ["abcd" "abcdef" "abc"])

;;15 3 Sum
;;Given an array S of n integers, are there elements a, b, c in S such that a + b + c = 0? Find all unique triplets in the array which gives the sum of zero.
(defn- all-pairs
  [v]
  (for [i (range 0 (count v))
        j (range (inc i) (count v))]
    [(v i) (v j)]))

(defn three-sum
  [v]
  (distinct
    (for [i (range 0 (count v))
          j (all-pairs (vec (concat (subvec v 0 i) (subvec v (inc i) (count v)))))
          :when (zero? (apply + (conj j (v i))))]
      (sort (conj j (v i))))))
#_(three-sum [-1, 0, 1, 2, -1, -4])

;;16
;;Given an array S of n integers, find three integers in S such that the sum is closest to a given number, target. Return the sum of the three integers. You may assume that each input would have exactly one solution.

(defn all-three-sums
  [v]
  (distinct
    (for [i (range 0 (count v))
          j (all-pairs (vec (concat (subvec v 0 i) (subvec v (inc i) (count v)))))]
      [(sort (conj j (v i))) (apply + (conj j (v i)))])))

(defn three-sum-closest
  [v t]
  (->> (all-three-sums v)
       (min-key #(Math/abs (- (second %) t)))
       first
       second))


#_(three-sum-closest [-1 2 1 -4] 1)

;;17 Letter combinations of a phone number
(defn combine-pairs
  [p1 p2]
  (for [x p1
        y p2]
    (str x y)))
(def tel-phone-letters
  {\2 ["a" "b" "c"]
   \3 ["d" "e" "f"]
   \4 ["g" "h" "i"]
   \5 ["j" "k" "l"]
   \6 ["m" "n" "o"]
   \7 ["p" "q" "r" "s"]
   \8 ["t" "u" "v"]
   \9 ["w" "x" "y" "z"]})

(defn letter-combos
  [tel]
  (reduce
    (fn [result letter] (combine-pairs result (get tel-phone-letters letter)))
    (get tel-phone-letters (first tel))
    (rest tel)))

;;18 4Sum
;;Given an array S of n integers, are there elements a, b, c, and d in S such that a + b + c + d = target? Find all unique quadruplets in the array which gives the sum of target.
;;I know what im going to do so im not going to do it get all 3 sums excluding the single char add up the char and then present the first one as an answer
;;(defn four-sum)



;; 209 Min size subarray sum
(defn min-size-subarray-sum
  [s nums]
  (apply min
         (for [i (range 0 (count nums))
               j (range i (count nums))
               :let [_ (prn i j (apply + (subvec nums i j)))]
               :when (>= (apply + (subvec nums i j)) s)]
           (dec (- j i)))))



;;150 RPN
(defn do-op
  [op t1 t2]
  (println op t1 t2)
  (case op
    "+" (+ (Integer/parseInt t1) (Integer/parseInt t2))
    "-" (- (Integer/parseInt t1) (Integer/parseInt t2))
    "*" (* (Integer/parseInt t1) (Integer/parseInt t2))
    "/" (int (/ (Integer/parseInt t2) (Integer/parseInt t1)))))

(defn is-op?
  [op]
  (#{"+" "-" "*" "/"} op))

(defn rpn
  ([tokens]
   (rpn tokens []))
  ([tokens stack]
   (println "stack" stack)
   (if (empty? tokens)
     (Integer/parseInt (peek stack))
     (if (is-op? (first tokens))
       (let [t1 (peek stack)
             stack (pop stack)
             t2 (peek stack)
             stack (pop stack)]
         (println stack)
         (recur (rest tokens)
                (->> (do-op (first tokens) t1 t2)
                     (str)
                     (conj stack))))
       (recur (rest tokens) (conj stack (first tokens)))))))


#_(rpn ["2", "1", "+", "3", "*"])
#_(rpn ["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"])


;;787 Cheapest flights within k stops
(defn make-graph
  [flights]
  (reduce
   (fn [graph [s d p]]
     (if (not (graph s)) (assoc graph s [[d p]])
         (assoc graph s (conj (graph s) [d p]))))
   {}
   flights))

(defn get-price
  [graph s d]
  (->> (graph s) (filter (fn [[dest _]] (= dest d))) first second))

(defn get-neighbors 
  [graph source]
  (map first (graph source)))

(defn set-cheapest-price
  [graph prices src dest]
  (assoc prices dest (min (prices dest) (+ (prices src) (get-price graph src dest)))))

(defn set-cheapest-prices
 [graph src cheapest-prices]
 (reduce (fn [acc d] (set-cheapest-price graph acc src d)) 
         cheapest-prices 
         (get-neighbors graph src)))

;;Uses djikstra and level ordered depth search
(defn cheapest-flight 
  [flights src dst k]
  (let [graph (make-graph flights)]
    (loop [queue (into clojure.lang.PersistentQueue/EMPTY [src])
           newset (into clojure.lang.PersistentQueue/EMPTY [])
           depth 0
           cheapest-prices (assoc (reduce (fn [acc k] (assoc (assoc acc k (Integer/MAX_VALUE)) (ffirst (graph k)) Integer/MAX_VALUE)) {} (keys graph)) src 0)]
      (println cheapest-prices dst)
      (cond
        (and (empty? queue) (empty? newset)) (if (= Integer/MAX_VALUE (cheapest-prices dst)) -1 (cheapest-prices dst))
        (< depth k) (if (empty? queue)
                      (recur newset (into clojure.lang.PersistentQueue/EMPTY []) (inc depth) cheapest-prices)
                      (recur (pop queue) (into newset (get-neighbors graph (peek queue))) depth (set-cheapest-prices graph (peek queue) cheapest-prices)))
        :else (recur (pop queue) newset depth (set-cheapest-prices graph (peek queue) cheapest-prices))))))

#_(cheapest-flight [[0,1,100],[1,2,100],[0,2,500]] 0 2 1)
#_(cheapest-flight [[0,1,100],[1,2,100],[0,2,500]] 0 2 0)
#_(cheapest-flight [[0,1,100],[1,2,100],[2,3,500]] 1 3 2)
#_(cheapest-flight [[0,1,100],[1,2,100],[1,3,500]] 0 2 0)