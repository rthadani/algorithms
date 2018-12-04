(ns algorithms.epi.recursive)

(defn hanoi
  [n l r m]
  (when (> n 0)
    (hanoi (dec n) l m r)
    (println (str "move " n  " from " l " to " r))
    (hanoi (dec n) m r l)
    ) )

#_ (hanoi 3 "l" "r" "m")


(defn phone-mnemonic
  [dictionary digits]
(if (empty? digits)
  [[]]
  (let [ digit (first digits)
        remaining (phone-mnemonic dictionary (rest digits))]
    (for [i (get dictionary digit)
          r remaining]
      (cons i r)))))

(def dictionary {\1 "" \2 "ABC" \3 "DEF" \4 "GHI" \5 "JKL" \6 "MNO" \7 "PQRS" \8 "TUV" \9 "WXYZ"})
#_ (count (phone-mnemonic dictionary "2276696"))

(defrecord Queen [row col])

(defn- attacked?
  [{q1row :row q1col :col} {q2row :row q2col :col}]
  (or
      (= q1row q2row)
      (= q1col q2col)
      (= (Math/abs (- q1row q2row)) (Math/abs (- q1col q2col)))))

(defn- place-safely?
  [board queen]
  (or (empty? board)
      (not-any? (partial attacked? queen) board)))

(defn- place-queens*
  [row size]
  (if (= 0 row)
    [[]]
    (let [solutions (place-queens* (dec row) size)
          possible-positions (map #(->Queen row %) (range 1 (inc size)))]
      (for [queen possible-positions
            board solutions
            :when (place-safely? board queen)]
        (conj board queen)))))

(defn place-queens
  [board-size]
  (place-queens* board-size board-size))

#_ (place-queens 8)


(defn permutations
  [arr]
  (if (empty? arr)
    [[]]
    (let [ f (first arr)
          remaining (permutations (rest arr))]
      (if (empty? (first remaining))
        [[f]]
        (into [] (for [p remaining
                       i (range 0 (inc (count p)))]
                   (if (< i (count p))
                     (into [] (concat (subvec p 0 i) [f] (subvec p i (count p))))
                     (conj p f))))))))
#_ (permutations [1 2 3])

(defn power-set
  [arr]
  (if (empty? arr)
    [[]]
    (let [ f (first arr)
          remaining (power-set (rest arr))]
      (into [] (concat remaining 
                       (for [p remaining]
                         (conj p f)))))))
#_ (power-set [1 2])
