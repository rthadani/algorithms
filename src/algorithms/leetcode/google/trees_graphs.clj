(ns algorithms.leetcode.google.trees-graphs
  (:require [algorithms.trees :refer :all]))

(defn max-path-sum
  [root]
  (if 
    (nil? root) 0
    (max (+ (:v root) (max-path-sum (:l root)) (max-path-sum (:r root)))
         (max-path-sum (:l root))
         (max-path-sum (:r root)))))

#_ (max-path-sum (create-tree  [-10,9, nil, nil, 20,15, nil, nil, 7, nil, nil]))



;;word ladder
(defn make-word-graph
  [dictionary]
  (into {} (map (fn [[k v]] [k (mapv second v)]) (group-by first
                                                       (for [word dictionary
                                                             i (range 0 (count word))
                                                             c  (map char (range (int \a) (inc (int \z))))
                                                             :let [new-word (str (subs word 0 i) c (subs word (inc i) (count word)))]
                                                             :when (and (not= new-word word ) (some #{new-word} dictionary))]
                                                         [word  new-word])))))

#_ (make-word-graph  ["hot","dot","dog","lot","log","cog"])

(defn word-ladder
  ([dictionary start end]
   (word-ladder (make-word-graph dictionary) (into clojure.lang.PersistentQueue/EMPTY [start]) end #{start} 0))
  ([graph q end used level]
   (cond (empty? q) -1
         (= end (peek q)) level
         :else
         (let [next-word (peek q)
               new-words (filter #(and (not (used %)) (not ((into #{} q) %))) (graph next-word))]
           (recur graph (into (pop q) new-words) end (conj used next-word) (inc level))))))

#_ (word-ladder  ["hit", "hot","dot","dog","lot","log","cog"] "hit" "cog")

;;number of islands
(defn flood-island-with-water
  [grid row col]
  (cond
    (or (< row 0) (< col 0)) grid
    (or (>= row (count grid)) (>= col (count (grid 0)))) grid
    (= "1" (get-in grid [row col])) (let [new-grid (assoc-in grid [row col] "0")] 
                                    (-> (flood-island-with-water new-grid (dec row) col)
                                       (flood-island-with-water row (dec col))
                                       (flood-island-with-water (inc row) col)
                                       (flood-island-with-water row (inc col))))
    :else grid))

  (defn number-of-islands
    ([grid] (last (flatten (number-of-islands grid 0))))
    ([grid result]
     (if (every? #(= "0" %) (flatten grid))
       result
       (for [row (range (count grid))
             col (range (count (grid 0)))
             :when (not (= "0" (get-in grid [row col])))]
         (number-of-islands (flood-island-with-water grid row col) (inc result))))))

#_ (flood-island-with-water [["1","1","0","0","0"]
                             ["1","1","0","0","0"]
                             ["0","0","1","0","0"]
                             ["0","0","0","1","1"]] 0 0)
#_ (number-of-islands [["1","1","0","0","0"]
                    ["1","1","0","0","0"]
                    ["0","0","1","0","0"]
                    ["0","0","0","1","1"]])

;;course schedule 2
(defn in-degree
  [course-dependencies]
  (reduce
    (fn [in-degree [dependent _]]
         (update in-degree dependent (fnil inc 0)))
    {}
    course-dependencies))
#_ (in-degree [[1,0],[2,0],[3,1],[3,2]])

(defn update-in-degree
  [degrees dependencies dependee]
  (reduce 
   (fn [degrees [dependant d]]
     (if (= d dependee) 
       (update degrees dependant dec) 
       degrees))
   degrees
   dependencies))

(defn execution-nodes 
  [degrees currently-executed]
  (->> (filter #(zero? (second %)) degrees)
       (remove #(currently-executed (first %)))
       (map first)))

#_ (execution-nodes {1 0 2 1} #{} )
#_ (execution-nodes {1 0 2 1} #{1} )

(defn course-schedule
  [num-courses dependencies]
  (let [dependency-counts (merge (into {} (map (fn [d] [(second d) 0]) dependencies)) (in-degree dependencies))]
      (loop [q (into (clojure.lang.PersistentQueue/EMPTY) (execution-nodes dependency-counts #{}))
             seen (into #{} (execution-nodes dependency-counts #{}))
             in-degree dependency-counts
             result []]
        (if (empty? q)
          result
          (let [next-degrees  (update-in-degree in-degree dependencies (peek q))
              free (execution-nodes next-degrees seen)]
            (recur (into (pop q) free)
                   (into seen free)
                   next-degrees
                   (conj result (peek q))))))))

#_ (course-schedule 2 [[1,0]])
#_ (course-schedule 4, [[1,0],[2,0],[3,1],[3,2]])

;;count complete tree nodes
(defn count-nodes
  [root]
  (cond
    (nil? root) 0
    :else (+ 1 (count-nodes (:l root)) (count-nodes (:r root)))))

;;try the binary search version
#_ (count-nodes (create-tree [1 2 4 5 3 6 nil]) )

;;longest increasing path

(defn path-size 
  [grid x y]
  (let [next-x (when (< (inc x) (count grid)) (get-in grid [(inc x) y]))
        prev-x (when (>= (dec x) 0) (get-in grid [(dec x) y]))
        next-y (when (< (inc y) (count (grid 0))) (get-in grid [x (inc y)]))
        prev-y (when (>= (dec y) 0) (get-in grid [x (dec y)]))
        current (get-in grid [x y])]
    (max
     (or (and next-x (> next-x current) (inc (path-size grid (inc x) y))) 0)
     (or (and prev-x (> prev-x current) (inc (path-size grid (dec x) y))) 0)
     (or (and next-y (> next-y current) (inc (path-size grid x (inc y)))) 0)
     (or (and prev-y (> prev-y current) (inc (path-size grid x (dec y)))) 0))))

(defn longest-increasing-path
  [grid]
  (inc (apply max (for [i (range (count grid))
                        j (range (count (grid 0)))]
                    (path-size grid i j)))))
  
  #_ (longest-increasing-path [ [3,4,5],
                               [3,2,6],
                               [2,2,1]] )
  
  
 #_ (longest-increasing-path [
                              [9,9,4],
                              [6,6,8],
                              [2,1,1]
                              ]) 

;;encoded string
(defn break-string
  [s]
  (println s)
  (let [regex #"(\d+)\[(([a-z]+)(.*?))\]?"]
    (if (.startsWith s "]")
      (re-matches regex (subs s 1))
      (re-matches regex s))))
#_ (break-string "3[a]2[bc]3[c]")
#_ (break-string "3[a2[c]]")
#_ (break-string "3[a2[c2[b]]]")
#_ (break-string "]2[bc")
(defn digit? 
  [char]
  (and (>= (int char) 48) (< (int char) 58)))

#_(defn encoded-string
  [s]
  (if (empty? s)
    ""
    (let [[_ repeats _  chars next-part] (break-string s)]
      (str (apply str (repeatedly (Integer/parseInt repeats) (constantly chars))) (encoded-string next-part)))))

#_ (encoded-string "3[a]2[bc]3[c]")
#_ (encoded-string "3[a2[c]]")
#_ (encoded-string "abc2[a]2[bc]")

(defn encoded-string
  ([s]
   (encoded-string s "" 0 []))
  ([s r c c-stack]
   (println r c-stack)
   (cond
     (empty? s) r
     (= \] (first s)) (str (apply str (repeat (first c-stack) r)) (encoded-string (subs s 1) "" 0 (rest c-stack)))
     (= \[ (first s)) (encoded-string (subs s 1) r 0 (cons c c-stack))
     (digit? (first s)) (str r (encoded-string (subs s 1) "" (+ (* c 10) (- (int (first s)) 48)) c-stack))
     :else (encoded-string (subs s 1) (str r (first s)) c c-stack))))

;;evaluate division
(defn build-equations-graph
  [equations values]
  (reduce (fn [g [[n d] v]]
            (-> (update g n conj [d v])
               (update d conj [n (/ 1 v)]))) 
   (reduce (fn[i [n d]] (assoc (assoc i n []) d [])) {} equations) 
   (zipmap equations values)))

(defn solve-equation
  [s t g seen]
  (cond
  (or (seen s) (not (contains? g s))) -1
  (= s t) 1
  :else (reduce
         (fn [acc [d v]] (let [eq (solve-equation d t g (conj seen s))]
                           (if (= eq -1)
                             acc
                             (reduced (* v eq)))))
         -1
         (get g s))))

(defn calc-equations 
  [equations values queries]
  (let [g (build-equations-graph equations values)]
    (for [[n d] queries]
      (solve-equation n d g #{}))))

#_ (build-equations-graph [ ["a", "b"], ["b", "c"] ] [2.0, 3.0])
#_ (calc-equations [ ["a", "b"], ["b", "c"] ]
                   [2.0, 3.0]
                   [ ["a", "c"], ["b", "a"], ["a", "e"], ["a", "a"], ["x", "x"] ])

;;Diameter of the tree
(defn tree-diameter
  [root]
  (if (nil? root)
    0
    (let [l (tree-diameter (:l root))
         r (tree-diameter (:r root))]
      (max (+ l r) (inc (max l r))))))

#_ (tree-diameter (create-tree [1 2 4 nil nil 5 nil nil 3 nil nil ]))

;;cracking the safe
;;
(defn crack-safe-dfs
  [node]
  )
(defn crack-safe
  [n k]
  )


;;robot room cleaner
(defprotocol Robot
  (turn-right [this])
  (turn-left [this])
  (move [this])
  (clean [this]))

#_(defrecord RobotImpl [grid facing x y cleaned-cells]
  Robot
  (turn-right 
   [_]
   (RobotImpl. grid (case facing
             :n :e
             :e :s
             :s :w
             :w :n) x y cleaned-cells))
  (turn-left
   [_]
   (RobotImpl. grid (case facing
                  :n :w
                  :w :s
                  :s :e
                  :e :n) x y cleaned-cells))
  (move
   [_]
   (RobotImpl. grid facing 
           (condp = facing
             :e (if (and (< x (dec (count (grid 0)))) (not (zero? (get-in grid [(inc x) y])))) (inc x) x)
             :w (if (and (> x 0) (not (zero? (get-in grid [(dec x) y])))) (dec x) x)
             x)
           (condp = facing
             :n (if (and (> y 0) (not (zero? (get-in grid [x (dec y)])))) (dec y) y)
             :s (if (and (< y (dec (count grid))) (not (zero? (get-in grid [x (inc y)])))) (inc y) y)
             y) cleaned-cells))
  (clean
   [_]
   (RobotImpl. grid facing x y (conj cleaned-cells [x y]))))

(defn turn-back
  [robot]
  (->> (turn-right robot)
       turn-right
       move))

(defn turn-right-n
  [robot n]
  (reduce #(turn-right %)
          robot
          (range 0 n)))

(defn cleaned? 
  [robot]
  (every? (fn [cell] ((:cleaned-cells robot) cell)) 
          (for [i (range (count (:grid robot)))
                j (range (count ((:grid robot) 0)))
                :when (= 1 (get-in (:grid robot) [i j]))]
            [j i])))

(defn robot-room-cleaner
  [robot moves]
  (if (cleaned? robot)
    true
    (let [dirs {:n [0 -1] :e [1 0] :s [0 1] :w [-1 0]}
          robot (clean robot)
          unclean-adjacent-cells (filter (fn [next-x next-y] (and (not ((:cleaned-cells robot) [next-x next-y])) (not= 0 (get-in (:grid robot) [next-y next-x])))) 
                                         (map (fn [[o-x o-y]] [(+ (:x robot) o-x) (+ (:y robot) o-y)]) (vals dirs)))]
      (if (seq unclean-adjacent-cells)
        (for [d (range 0 4)
              :let [new-state (move (turn-right-n robot d))]
              :when ((into #{} unclean-adjacent-cells) [(:x new-state) (:y new-state)] )]
          (robot-room-cleaner (turn-right-n robot d) moves))
        (robot-room-cleaner (turn-back robot) moves)))))

;;most stones removed
(defn make-stone-graph
  [stones]
  (->>
   (for [i (range 0 (count stones))
         j (range 0 (count stones))
         :when (and (not= i j) (or (= (first (stones i)) (first (stones j))) (= (second (stones i)) (second (stones j)))))]
     [(stones i) (stones j)])
   (group-by first)
   (map (fn [[k vs]] [k (mapv second vs)]))
   (into {})))

(defn remove-stone-deps
  [stones g]
  (reduce (fn [new-g s]  (dissoc new-g s)) g stones))

(defn find-max-stone-key
  [g]
  (key (apply max-key #(count (val %)) g)))

(defn remove-stones
  [stones]
  (loop [g (make-stone-graph stones)
         c 0]
    (if (zero? (count g))
      c
      (let [max-stone-key (find-max-stone-key g)
            next-g (remove-stone-deps (get g max-stone-key) g)]
        (recur (dissoc next-g max-stone-key) (+ c (count (get g max-stone-key))))))))
  
#_ (remove-stones [[0, 0]])
#_ (def g (make-stone-graph [[0,0],[0,1],[1,0],[1,2],[2,1],[2,2]]))
#_ (remove-stone-deps  (get g (find-max-stone-key g)) g)
#_ (remove-stones [[0,0],[0,2],[1,1],[2,0],[2,2]])

(defn flip-equivalent-tree
  [root1 root2]
  (cond 
    (= root1 root2) true
    (or (nil? root1) (nil? root2) (not= (:v root1) (:v root2))) false
    :else (or (and (flip-equivalent-tree (:l root1) (:l root2))
                (flip-equivalent-tree (:r root1) (:r root2)))
              (and (flip-equivalent-tree (:l root1) (:r root2))
                 (flip-equivalent-tree (:r root1) (:l root2))))))
#_ (flip-equivalent-tree (create-tree [1 2 4 nil nil 5 7 nil nil 8 nil nil 3 6 nil])
                         (create-tree [1 3 nil 6 nil nil 2 4 nil nil 5 8 nil nil 7 nil nil])) 


