(ns algorithms.sudoku
  (:require [clojure.set :as set]))

(def all-values (range 1 10) )
(defn rows-complete?
  [grid]
  (every? #(= (sort %) all-values) grid))

(defn transpose
  [grid]
  (apply mapv vector grid))

(defn columns-complete? [grid]
  (->> (transpose grid)
       rows-complete?))

(defn box-for-position
  [grid x y]
  (let [start-row (* (int (/ y 3)) 3)
        start-col (* (int (/ x 3)) 3)]
    [(subvec (get grid start-row) start-col (+ 3 start-col))
     (subvec (get grid (inc start-row)) start-col (+ 3 start-col))
     (subvec (get grid (+ 2 start-row)) start-col (+ 3 start-col))]))

(defn make-boxes
  [grid]
  (into [] (for [i (range 0 3)
                 j (range 0 3)
                 :let [start-row (* 3 i)
                       start-col (* 3 j)]]
             (box-for-position grid start-col start-row))))

(defn boxes-complete?
  [grid]
  (->> (make-boxes grid)
     (every? #(= (sort (flatten %)) all-values))))


(defn solved?
  [grid]
  (and (rows-complete? grid)
       (columns-complete? grid)
       (boxes-complete? grid)))

(defn row-values
  [grid y]
  (->> (get grid y)
       (filter #(not (zero? %)))
       set))

(defn col-values
  [grid x]
  (-> (transpose grid)
      (row-values x)))

(defn box-values
  [grid x y]
  (->> (box-for-position grid x y)
        (flatten)
        (filter #(not (zero? %)))
        set))

(defn find-empty-spot [grid]
  (first (for [y (range 9)
               x (range 9)
               :when (zero? (get-in grid [y x]))]
           [x y])))

(defn possible-values-for-spot [grid [x y]]
  (set/difference (into #{} all-values)
                  (set/union (row-values grid y) (col-values grid x) (box-values grid x y))))

(defn- solve*
  [grid]
  (if (solved? grid)
    [grid]
    (let [spot (find-empty-spot grid)
          possible-solutions (possible-values-for-spot grid spot)]
      (for [value possible-solutions
            solution (solve* (assoc-in grid (reverse spot) value))]
        solution))))

(defn solve
  [grid]
  (first (solve* grid)))

(def board
  [[5 3 0 0 7 0 0 0 0]
   [6 0 0 1 9 5 0 0 0]
   [0 9 8 0 0 0 0 6 0]
   [8 0 0 0 6 0 0 0 3]
   [4 0 0 8 0 3 0 0 1]
   [7 0 0 0 2 0 0 0 6]
   [0 6 0 0 0 0 2 8 0]
   [0 0 0 4 1 9 0 0 5]
   [0 0 0 0 8 0 0 7 9]])

(def hardest
  [[8 5 0 0 0 2 4 0 0]
   [7 2 0 0 0 0 0 0 9]
   [0 0 4 0 0 0 0 0 0]
   [0 0 0 1 0 7 0 0 2]
   [3 0 5 0 0 0 9 0 0]
   [0 4 0 0 0 0 0 0 0]
   [0 0 0 0 8 0 0 7 0]
   [0 1 7 0 0 0 0 0 0]
   [0 0 0 0 3 6 0 4 0]])