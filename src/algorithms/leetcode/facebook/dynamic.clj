(ns algorithms.leetcode.facebook.dynamic)

(declare decode-ways)

(defn decode-ways*
  [s]
  (cond
    (empty? s) 1
    (= \0 (first s)) 0
    (= (count s) 1) 1
    (<= (Integer/parseInt (subs s 0 2)) 26) (+ (decode-ways (subs s 1)) (if (> (count s) 2)  (decode-ways (subs s 2)) 1))
    :else (decode-ways (subs s 1))))

(def decode-ways (memoize decode-ways*))

#_ (decode-ways "12")
#_ (decode-ways "226")

(declare word-break)
(defn word-break*
  ([s dict]
   (word-break s dict 0))
  ([s dict i]
   (if (= i (count s)) 
     true
     (reduce
      (fn [_ end]  (if (and (contains? dict (subs s i end)) (word-break s dict end))
                       (reduced true)
                       false))
      false
      (range (inc i) (inc (count s)))))))

(def word-break (memoize word-break*))
#_ (word-break "leetcode" #{"leet", "code"})
#_ (word-break "leetcode" #{"leet", "code"})
#_ (word-break "catsandog", ["cats", "dog", "sand", "and", "cat"])

(defn longest-valid-parenthesis
  [s]
  (loop [result 0
         i 0
         stack [-1]]
    (if (= i (count s)) 
      result
      (cond 
        (= (nth s i) \() (recur result (inc i) (conj stack i))
        (= 1 (count stack)) (recur result (inc i) (conj (pop stack) i))
        :else (recur (max result (- i (peek (pop stack)))) (inc i) (pop stack)))))


  #_(if (< (- (count s) i) 2)
      0
      (max
       (if (and (= (nth s i) \() (nth s (inc i) \))) (+ 2 (longest-valid-parenthesis s (+ 2 i))) (longest-valid-parenthesis s (inc i)))
       (if (and (= (nth s i) \() (nth s (inc i) \() (=  \) (nth s (+ i (longest-valid-parenthesis s (inc i))))))
         (+ (longest-valid-parenthesis s (inc i)) (longest-valid-parenthesis s (+ i (longest-valid-parenthesis s (inc i)) 2)))
         (longest-valid-parenthesis s (inc i))))))

#_ (longest-valid-parenthesis "(()")
#_ (longest-valid-parenthesis ")()()())" )

