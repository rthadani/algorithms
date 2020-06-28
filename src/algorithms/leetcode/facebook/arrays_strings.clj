(ns algorithms.leetcode.facebook.arrays-strings
  (:require [clojure.string :as string]))

;;longest substring without repeats
(defn longest-substring-no-repeats
  ([s]
   (longest-substring-no-repeats s #{} 0 0 0))
  ([s current-set  l r current-count]
   (cond
     (or (>= l (count s)) (>= r (count s))) current-count
     (current-set (nth s r)) (max current-count
                                  (longest-substring-no-repeats s (disj current-set (nth s l)) (inc l) r (- r l)))
     :else (longest-substring-no-repeats s (conj current-set (nth s r)) l (inc r) (- (inc r) l)))))

#_ (longest-substring-no-repeats "abcabcbb")
#_ (longest-substring-no-repeats "bbbbb")
#_ (longest-substring-no-repeats "abcbdef")
#_ (longest-substring-no-repeats "pwwkew")

;;String to integer
(defn my-atoi
  ([s] (my-atoi s 0 false 1))
  ([s result sign-seen sign]
   (cond
     (empty? s) (* sign result)
     (and (= \space (first s)) (zero? result) (not sign-seen)) (my-atoi (rest s) result sign-seen sign)
     (and (or (= \+ (first s)) (= \- (first s))) (zero? result) (not sign-seen)) (my-atoi (rest s) result true (if (= \+ (first s)) 1 -1))
     (and (>= (int (first s)) 48) (<= (int (first s)) 57)) (my-atoi (rest s) (+ (* 10 result) (- (int (first s)) 48)) sign-seen sign)
     :else (* sign result))))

#_ (my-atoi "42")
#_ (my-atoi "      -42")
#_ (my-atoi "-4123 with words")
;;roman to integer
(def numerals {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000})
(defn decimal-values [s] (map numerals s))
(defn combine [r [c n]] (if (nil? n) (+ r c) (if (>= c n) (+ r c) (- r c))))
(defn roman-to-integer
  [s]
  (reduce combine 0 (partition-all 2 1 (map numerals s))))

#_ (roman-to-integer "MCMXCIV")
#_ (roman-to-integer "LVIII")

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

;;remove duplicates from sorted array
(defn remove-duplicates
  [a]
  (reduce #(if (not= (last %) %2) (conj % %2) %)  [] a))
#_ (remove-duplicates [1 1 2])

;;next-permutation
(defn find-first-non-decreasing-element-idx
  [a]
  (->> (partition-all 2 1 (reverse a))
       (reduce (fn [i [n p]] (if (> n p) (reduced (inc i)) (inc i))) 0)
       (- (count a) 1)))

#_ (find-first-non-decreasing-element-idx [1 2 3 4])
(defn find-swap-index
  [p i]
  (dec (reduce (fn [r c]
                 (if (< c (p i))
                   (reduced (+ i r 1))
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
        _ (println n (find-swap-index p n))
        swapped (if (>= n 0) (swap-idx p n (find-swap-index p n)) p)]
    (println swapped)
    (concat (subvec swapped 0 (inc n)) (reverse (subvec swapped (inc n))))))

#_ (next-permutation [1 5 8 4 7 6 5 3 1])
#_ (next-permutation [1 2 3])

;;string multiplication
(defn convert-to-digits
    [num]
    (if (empty? num)
      []
      (cons (*' (Math/pow 10 (dec (count num))) (Integer/parseInt (str (first num))))
            (convert-to-digits (rest num)))))

(defn multiply-arrays
  [ns1 ns2]
  (for [n1 ns1
        n2 ns2]
    (*' n1 n2)))

(defn multiply-strings
  [num1 num2]
  (let [ns1 (convert-to-digits num1)
        ns2 (convert-to-digits num2)]
    (println ns1 ns2 (multiply-arrays ns1 ns2))
    (->> (multiply-arrays ns1 ns2)
         (apply +')
         (bigint)
         str)))

#_ (multiply-strings "123456789" "987654321")

;;group anagrams
(defn group-anagrams
  [input]
   (->> (map (fn [w] [(sort w) w]) input)
        (group-by first)
        vals
        (map #(map second %))))

#_ (group-anagrams ["eat", "tea", "tan", "ate", "nat", "bat"])

;;add binary
(defn add-bit [a b c]
  (let [sum (+ a b c)]
    (if (>= sum 2) [(- sum 2) 1] [sum 0])))

(defn add-binary
  ([a b]
   (add-binary (map #(Integer/parseInt (str %))  (reverse a)) (map #(Integer/parseInt (str %)) (reverse b)) 0))
  ([a b c]
   (cond
     (and (empty? a) (empty? b)) [c]
     (empty? a)  (let [[s c] (add-bit 0 (first b) c)] (conj (add-binary a (rest b) c) s))
     (empty? b)  (let [[s c] (add-bit (first a) 0 c)] (conj (add-binary (rest a) b c) s))
     :else  (let [[s c] (add-bit (first a) (first b) c)] (conj (add-binary (rest a) (rest b) c) s)))) )

  #_ (add-binary "11" "1")
  #_ (add-binary "11" "0")

(defn is-available?
  [freq] 
  (every? #(<= (second %) 0) freq))

;;min window substring
(defn min-window-substring
  ([s t]
   (min-window-substring s (frequencies t) 0))
   ([s f _] 
    (loop [i 0
           j 0
           ans Integer/MAX_VALUE
           f f]
      (if (and (= i j) (= j (dec (count s))))
        ans
        (cond
          (is-available? f) (recur (inc i) j (min ans (- j i)) (if (contains? f (nth s i)) (update f (nth s i) inc) f))
          (= j (dec (count s))) (recur (inc i) j ans (if (contains? f (nth s i)) (update f (nth s i) inc) f))
          (contains? f (nth s j)) (recur i j ans (update f (nth s i) dec))
          :else (recur i (if (< j (count s)) (inc j) j) ans f))))))

#_ (min-window-substring "ADOBECODEBANC" "ABC")

(defn valid-palindrome 
  [s]
  (as->
   (string/replace s #"[^\w]" "") $
   (string/lower-case $)
   (= $ (string/reverse $))))

#_  (valid-palindrome "A man, a plan, a canal: Panama")

;;one edit distance
(defn is-one-edit-distance?
  [s t]
  (cond
    (empty? s) (count t)
    (empty? t) (count s)
    :else
     (min (+ 1 (is-one-edit-distance? (rest s) t))
          (+ 1 (is-one-edit-distance? s (rest t)))
          (+ (if (= (first s) (first t)) 0 1) (is-one-edit-distance? (rest s) (rest t))))))

#_ (is-one-edit-distance? "ab" "acb")
#_ (is-one-edit-distance? "cab" "ad")

(defn product-except-self
  [a]
  (let [L (butlast (reduce (fn [r a] (conj r (* a (last r)))) [1] a))
        R (rest (reverse (reduce (fn [r a] (conj r (* a (last r)))) [1] (reverse a))))]
    (map #(* %1 %2) L R)))

#_ (product-except-self [4 5 1 8 2])
#_ (product-except-self [1 2 3 4])
(def words {:ones ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]
            :teens ["ten" "eleven" "twelve" "thriteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"]
            :tens ["ten" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"]
           } )

(defn build-10s
  [i]
  (cond
    (>= i 20) (str ((:tens words) (dec (quot i 10))) " " (build-10s (mod i 20)))
    (>= i 10) (str ((:teens words) (dec (quot i 10))) " " (build-10s (mod i 10)))
    (> i 0) ((:ones words) (dec i))
    :else ""))

(defn build-100s
  [i]
  (if (>= i 100)
    (str ((:ones words) (dec (quot i 100))) " hundred " (build-10s (int (mod i 100))))
    (build-10s i)))

(defn integer-to-english
  [i]
  (cond
    (>= i (Math/pow 10 9)) (str (build-10s (int (quot i (Math/pow 10 9)))) " billion " (integer-to-english (int (mod i (Math/pow 10 9)))))
    (>= i (Math/pow 10 6)) (str (build-100s (int (quot i (Math/pow 10 6)))) " million " (integer-to-english (int (mod i (Math/pow 10 6)))))
    (>= i 1000) (str (build-100s (int (quot i (Math/pow 10 3)))) " thousand " (integer-to-english (int (mod i (Math/pow 10 3)))))
    (>= i 100) (build-100s i)
    :else (build-10s i)))

#_ (integer-to-english 4567891210)

(defn move-zeroes
  [a]
  (cond 
    (empty? a) []
    (zero? (first a)) (vec (move-zeroes (rest a)))
    :else (cons (first a) (vec (move-zeroes (rest a))))))

#_ (move-zeroes [0,1,0,3,12])

(defn is-valid-ip6-part?
  [part]
  (and (<= (count part) 4)  (or (empty? part) (re-matches #"[0-9a-fA-F]+" part))))

(defn is-valid-ip4-part?
  [part]
  (println (re-matches #"25[0-5]|2[0-4][0-9]|[0-9]?[0-9]?|1[0-9]?[0-9]?" part))
  (and (<= (count part) 3) (not (empty? part)) (re-matches #"(25[0-5]|2[0-4][0-9]|1[0-9][0-9]?|[0-9])" part))) 

(defn is-ip6? 
  [ip]
  (= 7 (count (string/split ip #":"))))

(defn is-ip4?
  [ip]
  (println ip (count (string/split ip #"\.")))
  (= 4 (count (string/split ip #"\."))))

(defn is-valid-ip?
  [ip]
   (or (and (is-ip6? ip) (every? is-valid-ip6-part? (string/split (string/lower-case ip) #":")))
       (and (is-ip4? ip) (every? is-valid-ip4-part? (string/split (string/lower-case ip) #".")))))

#_ (is-valid-ip? "02001:0db8:85a3:0:8A2E:0370:7334")
#_ (is-valid-ip? "256.0.0.1")

;;num continuous subarrays whose sum = k
(defn sub-array-sum-equals-k
  [a k]
  (second (reduce  (fn [[sums c s] n]
              [(assoc sums (+ s n) (inc (get sums (+ n s) 0)))
               (if (contains? sums (- (+ n s) k))  (+ c (sums (- (+ n s) k))) c)
               (+ n s)]) [{0 1} 0 0] a)))

#_ (sub-array-sum-equals-k [1 1 1] 2)

;;is palindrome 2
(defn is-palindrome-range
  [s]
  )



