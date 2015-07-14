(ns algorithms.gol
  (require [clojure.core.match :refer [match]]))

(defn is-alive?
  [grid x y]
  (if (or (< x 0)
          (< y 0)
          (> y (count grid))
          (> x (count (grid 0))))
    false)
    (get-in grid x y))

(defn get-live-neighbours
  [grid x y]
  (apply sum (for [x-offset [-1 0 1]
                   y-offset [-1 0 1]
                   :when (not-every? zero? [x-offset y-offset])]
               (if (is-alive? grid
                              (+ x x-offset)
                              (+ y y-offset))
                 1
                 0))))

(defn get-cell-state
  [grid x y]
  (let [is-cell-alive (is-alive? grid x y)
        live-neighbours (get-live-neighbours grid x y)]
       (match [is-cell-alive live-neighbours]
              [true  (_ :guard #(< % 2))]              false
              [true  (_ :guard #(or (= 2 %) (= 3 %)))] true
              [true  (_ :guard #(> % 3))]              false
              [false (_ :guard #(= % 3))]              true)))

(defn get-next-step
  [grid]
  (map (fn [y]
         (map (fn [x]
                (get-cell-state grid x y))
              (range 0 (count (grid 0)))))
       (range 0 (count grid))))