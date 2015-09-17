(ns algorithms.maze)
;Given a maze
;  [[:S 0 1]
;  [1  0 1]
;  [1  0 :E]]
;S is the start of the maze
;E is the end of the maze
;1 is a wall that you cannot pass through
;0 is a free space that you can move through”

(defn start
  [maze]
  (first (for [i (range (count maze))
               j (range (count (get maze i)))
               :when (= :S (get-in maze [i j]))]
           [i j])))

(defn element-at
  [maze x y]
  (when (and maze (< x (count maze)) (< y (count (get maze x)))))
    (get-in maze [x y]))

(defn space-available?
  [maze x y]
  (when-let [element (element-at maze x y)]
    (= 0 element)))

(defn update-position
  [maze x y value]
  (assoc-in maze [x y] value))

(defn solve-maze*
  [maze [x y] solution]
  (when (nil? maze) nil)
  (if (= :E (or (element-at maze (inc x) y)
                (element-at maze x (inc y))))
    (swap! solution assoc-in [0] maze)
    (do
      (when (space-available? maze x (inc y))
        (-> (update-position maze x (inc y) :x)
            (solve-maze* [x (inc y)] solution)))
      (when (space-available? maze (inc x) y)
        (-> (update-position maze (inc x) y :x)
            (solve-maze* [(inc x) y] solution))))))

(defn solve-maze [maze]
  (let [solution (atom [])]
    (solve-maze* maze (start maze) solution)
    (first @solution)))

(defn print-maze
  [maze]
  (doall (map #(println %) maze)))

(print-maze (solve-maze [[:S 0 1]
                         [1 0 1]
                         [1 0 :E]]))
(println)
(print-maze (solve-maze [[:S 0 0 1]
                         [1 1 0 0]
                         [1 0 0 1]
                         [1 1 0 :E]]))

