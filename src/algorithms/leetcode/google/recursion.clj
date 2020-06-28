(ns algorithms.leetcode.google.recursion)

;;word squares
(defn solved-square?
  [square]
  (not-any? nil? (flatten square)))

(defn find-first-empty-spot
  [square]
  (first
   (for [r (range 0 (count square))
         c (range 0 (count (square 0)))
         :when (nil? (get-in square [r c]))]
     [r c])))

(defn find-words-for-empty-spot
  [words square [r c]]
  (let [prefix (apply str (subvec (square r) 0 c))]
    (filter #(.startsWith % prefix) words)))

(defn add-word-to-square
  [word square [r c]]
  (first (reduce (fn [[square i] char] [(-> (assoc-in square [r i] char)
                                            (assoc-in [i r] char)) (inc i)])
                 [square 0]
                 word)))

(defn initialize-square
  [words]
  (vec (for [_ (range 0 (count (first words)))]
         (vec (repeat (count (first words)) nil)))))

(defn solve-word-square
  ([words]
   (filter solved-square? (solve-word-square words (initialize-square words) [])))
  ([words square results]
   (if (solved-square? square)
     (conj results square)
     (let [empty-spot (find-first-empty-spot square)
           words-for-spot (find-words-for-empty-spot words square empty-spot)]
       (for [word words-for-spot
             solutions (solve-word-square words (add-word-to-square word square empty-spot) results)]
         solutions)))))

#_(def words ["ball","area","lead","lady"])
#_(solve-word-square words)
#_(solve-word-square ["area","lead","wall","lady","ball"])
#_ (solve-word-square ["abat","baba","atan","atal"])

;;strobogrammatic number II
(defn strobogrammatic-gen
  ([n] (strobogrammatic-gen n n))
  ([n m]
   (cond
     (zero? n) []
     (= 1 n) ["0" "1" "8"]
     :else
     (let [remaining (strobogrammatic-gen (- n 2) m)]
       (flatten (for [r remaining]
                  (->  (if (not= n m) [(str "0" r "0")])
                       (conj (str "1" r "1"))
                       (conj (str "6" r "9"))
                       (conj (str "9" r "6"))
                       (conj (str "8" r "8")))))))))

#_(strobogrammatic-gen 3)

;;word search II
;;android unlock patterns 

;;letter-combinations
(def phone-patterns {1 []
                     2 [\a \b \c]
                     3 [\d \e \f]
                     4 [\g \h \i]
                     5 [\j \k \l]
                     6 [\m \n \o]
                     7 [\p \q \r \s]
                     8 [\t \u \v]
                     9 [\w \x \y \z]
                     0 [\_]})

(defn letter-combinations
  [digits]
  (cond
    (empty? digits) []
    (= 1 (count digits)) (mapv #(str %) (phone-patterns (Integer/parseInt digits)))
    :else (let [remaining-combinations (letter-combinations (subs digits 1))
              first-pattern (phone-patterns (Integer/parseInt (str (first digits))))]
            (for [r remaining-combinations
                  f first-pattern]
              (str f r)))))

#_ (letter-combinations "23")
#_ (letter-combinations "234")

;;generate parenthesies
(defn insert-complete-parens
  [current]
  (reduce (fn [s c] (if (= c \() (str s c "()") (str s \)))) "" current))
#_ (insert-complete-parens "()")
(defn gen-parens
  [n]
  (into #{} (cond (zero? n) [""]
                  :else (let [remaining (gen-parens (dec n))
                              _ (println remaining n)
                              make-complete-parens (for [r remaining
                                                         i (range 0 (count r))
                                                         :when (= (nth r i) \()] 
                                                     (str (subs r 0 (inc i)) "()" (subs r (inc i))))]
                          (concat make-complete-parens (for [c remaining]
                                                         (str "()" c)))))))
#_ (gen-parens 3)

