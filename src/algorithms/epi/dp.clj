(ns algorithms.epi.dp)

(declare score-combinations)
(defn score-combinations*
  [score play-scores]
 (println score play-scores)
 (cond
   (zero? score) 1
   (or (empty? play-scores) (< score 0)) 0
   :else (let [with-play-1 (score-combinations (- score (first play-scores)) play-scores)
                with-play-2 (score-combinations (- score (first play-scores)) (rest play-scores))
               without-play (score-combinations score (rest play-scores))]
             (+ with-play-1 with-play-2 without-play))))
  
(def score-combinations (memoize score-combinations*))
#_ (score-combinations 12 [2 3 7])

(declare levenshtein)
(defn levenshtein*
  [w1 w2]
  (cond
    (zero? (count w1)) (do (println "finished w1") [(count w2) [(str "delete " (count w2) " chars from w2")]])
    (zero? (count w2)) (do (println "finished w2") [(count w1) [(str "delete " (count w1) " chars from w1")]])
    (= (last w1) (last w2))
    (let [[cost directions] (levenshtein (butlast w1) (butlast w2))]
      [cost (conj directions (str "skip " (last w1)))])
    :else
    (let [[substitute-cost sc-dirs] (levenshtein (butlast w1) (butlast w2))
          [insert-cost ins-dirs] (levenshtein w1 (butlast w2))
          [delete-cost del-dirs] (levenshtein (butlast w1) w2)
          min-cost (min substitute-cost insert-cost delete-cost)]
      (condp = min-cost
        substitute-cost [(inc min-cost) (conj sc-dirs (str "substitute letters " (last w1) " " (last w2)))]
        insert-cost [(inc min-cost) (conj ins-dirs (str "insert letter " (last w2) " in " w1))]
        delete-cost [(inc min-cost) (conj del-dirs (str "delete letter " (last w1)))]))))
(def levenshtein (memoize levenshtein*))
#_ (levenshtein (seq  "saturday") (seq "sunday"))


(declare num-ways)
(defn num-ways*
  [n m]
  (cond
    (and (zero? n) (zero? m)) 1
    (zero? n) (num-ways 0 (dec m))
    (zero? m) (num-ways 0 (dec n))
    :else (+ (num-ways n (dec m)) (num-ways (dec n) m))))

(def num-ways (memoize num-ways*))

#_ (num-ways 4 4)

(declare nCr)
(defn nCr* 
  [n r]
  (cond (= n r) 1
        (= r 1) n
    :else (+ (nCr (dec n) r) (nCr (dec n) (dec r)))))
(def nCr (memoize nCr*))

#_ (nCr 5 2)

(declare knapsack)
(defn knapsack*
  [weights values max-weight]
  (cond 
    (or (zero? max-weight) (empty? values)) 0
    (> (first weights) max-weight) (knapsack (rest weights) (rest values) max-weight)
    :else 
    (max (+ (first values) (knapsack (rest weights)
                                     (rest values)   
                                     (- max-weight (first weights))))
         (knapsack (rest weights) (rest values) max-weight))))

(def knapsack (memoize knapsack*))
#_ (knapsack [5 3 4 2]  [60 50 70 30] 5)

(defn decompose-words
  [s dictionary i]
  (cond 
    (empty? s) true
    (dictionary (str s)) true)
    :else (for [j (range 0 i)]))

