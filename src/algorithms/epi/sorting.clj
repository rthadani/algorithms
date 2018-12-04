(ns algorithms.epi.sorting)

(defn intersect-sorted-arrays
 [arr1 arr2]
 (loop [result #{}
        arr1 arr1
        arr2 arr2]
       (if (or (empty? arr1) (empty? arr2))
         result
         (let [a1 (first arr1)
               a2 (first arr2)]
           (cond (= a1 a2) (recur (conj result a1) (rest arr1) (rest arr2))
             (< a1 a2)  (recur result (rest arr1) arr2)
             :else (recur result arr1 (rest arr2))))))) 

#_ (intersect-sorted-arrays [2 3 3 5 5 6 7 7 8 12] [5 5 6 8 8 9 10 10])

(defn max-non-constructible-value
  [arr]
  (reduce
   (fn [max-constructible e] 
     (if (> e (inc max-constructible))
       (reduced (inc max-constructible))
       (+ max-constructible e)))
   0
   (sort arr)))
#_(max-non-constructible-value [12 2 1 15 2 4])

;;keep all that have simultaneously started and increment 
(declare split-start-end)
(declare compare-intervals)
(defn max-concurrent-events
  [cal]
  (->> coll 
       split-start-end
       (sort-by identity compare-intervals) 
       (reduce (fn [overlap [time is-start?]] 
                 (if is-start?
                   (inc overlap)
                   (dec overlap)))
               0)))
(defn- split-start-end
  [intervals]
  (mapcat (fn [interval] [(first interval) true] [(second interval) false]) intervals))

(defn- compare-interval
  [a b]
  (if (= (first a) (first b))
    (if (false? (second a)) 1 -1))
    (- a b))


(defn merge-intervals
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

#_ (merge-intervals [[-4 -1] [0 2] [3 6] [7 9] [11 12] [14 17]] [1 8])

