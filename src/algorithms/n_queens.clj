(ns algorithms.n-queens)

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

(defn print-board
  [board]
  (doseq [x (range 1 (inc (count board)))
          y (range 1 (inc (count board)))]
    (if (some #(and (= x (:row %)) (= y (:col %))) board)
      (print "x ")
      (print ". "))
    (when (= y (count board))
      (println "")))
 (println))

