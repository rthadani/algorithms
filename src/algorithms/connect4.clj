(ns algorithms.connect4)

(defn init-state
  [rows cols]
  ;;assert that the number of rows and columns > 4
  (let [board (vec (for [_ (range 0 rows)]
                     (vec (repeat cols nil))))
        row-tops (vec (repeat cols rows))]
    {:board board
     :row-tops row-tops
     :next-turn :red
     :winner nil
     :filled 0}))

(defn columns
  [board]
  (count (board  0)))

(defn next-row
  [game-state col]
  (-> (:row-tops game-state)
      (get col)
      (dec)))

(defn extract-horizontal 
  [board row col]
  (for [i (range -3 4)
        :let [start (+ i col) 
              end (+ 3 i col)]
        :when (and (>= start 0) (< end (columns board)))]
    (subvec (board row) start (inc end))))

(defn extract-vertical
  [board row col]
  (for [i (range -3 4)
        :let [start (+ i row)
              end (+ 3 i row)]
        :when (and (>= start 0) (< end (count board)))]
    (vec (for [j (range 0 4)]
           (get-in board [(+ start j) col])))))

(defn extract-desc-diagonal
  [board row col]
  (for [i (range -3 4)
        :let [start-row (+ i row)
              end-row (+ 3 start-row)
              start-col (+ i col)
              end-col (+ 3 start-col)]
        :when (and (>= start-row 0) (< end-row (count board)) (>= start-col 0) (< end-col (columns board)))]
    (vec (for [j (range 0 4)]
           (get-in board [(+ start-row j) (+ start-col j)])))))

(defn extract-asc-diagonal
  [board row col]
  (for [i (range -3 4)
        :let [start-row (+ i row)
              end-row (+ 3 start-row)
              start-col (+ (* i -1) col)
              end-col (- start-col 3)
              #_(println start-row " " start-col " " end-row " " end-col)]
        :when (and (>= start-row 0) (< end-row (count board)) (>= start-col 0) (< end-col (columns board)))]
    (vec (for [j (range 0 4)]
           (get-in board [(+ start-row j) (- start-col j)])))))

(defn extract-run
  [board row col]
  (let [h      (extract-horizontal board row col)
        v      (extract-vertical board row  col)
        dd     (extract-desc-diagonal board row  col)
        da     (extract-asc-diagonal board row col)]
    (concat h v dd da)) )

(defn winner? 
  [{:keys [board row-tops]} col color]
  (let [win-fn (fn [cf] (every? #(= color %) cf))
        runs (extract-run board (row-tops col) col)]
    (some #(win-fn %) runs)))

(defn draw?
  [{:keys [board filled]}]
  (= filled (* (count board) (count (board 0)))))

(defn play-coin
  [{:keys [board next-turn] :as game-state} col]
  {:pre [(>= col 0) (< col (count (board 0))) #_(>= (next-row game-state col) 0)]}
  (let [row-top (next-row game-state col)
        game-state (-> (update game-state :board #(assoc-in % [row-top col] next-turn))
                       (update :row-tops #(assoc % col row-top))
                       (update :next-turn #(if (= % :red) :yellow :red))
                       (update :filled inc))]
    (if
     (winner? game-state col next-turn) 
      (assoc game-state :winner next-turn)
     game-state)))

(defn draw-board
  [{:keys [board]}]
  (doseq [i    (range 0 (count board))
          j    (range 0 (count (board 0)))
          :let [slot (get-in board [i j])]]
    (cond 
      (nil? slot) (print "_ ")
      (= :red slot) (print "R ")
      :else (print "Y "))
    (when (= j (dec (count (board 0))))
      (println ""))))

(defn accept-input
  [color]
  (println (str "It is " (name color) "'s turn \n Enter column to play"))
  (Integer/parseInt (read-line)))



;;ai
(defn score-run
  [run c]
  (condp = run
    [c c c nil] 5
    [c c nil c] 5
    [c nil c c] 5
    [nil c c c] 5
    [nil nil c c] 2
    [c nil nil c] 2
    [c c nil nil] 2
    [nil c c nil] 3
    0))

(defn score-board
  [{:keys [board row-tops]} color]
  (->> (for [row (range (apply min row-tops) (count board))
             col (range (count (board 0)))
             :when (= color (get-in board [row col]))]
         (extract-run board row col))
       (apply concat)
       (reduce (fn [s r]  (+ s (score-run r color))) 0)))
    

(defn open-cols
  [row-tops]
  (filter #(> % 0) row-tops))

(defn minmax
  [{:keys [next-turn winner row-tops] :as gs} color depth]
  (cond
    winner (if (= winner next-turn) [nil 1] [nil -1])
    (draw? gs) [nil 0]
    :else (if (zero? depth)
            [nil (score-board gs next-turn)]
            (->> (for [col row-tops
                       :when (> (row-tops col) 0)
                       :let [[_ score] (minmax (play-coin gs col) color (dec depth))]]
                   [col (- score)])
                 (apply max-key second)))))


(defprotocol Player
  (make-move [this game-state]))

(defrecord HumanPlayer [id color]
  Player
  (make-move 
   [_ game-state]
   (->> (accept-input color)
        (play-coin game-state))))

(defrecord ComputerPlayer [id color]
  Player
  (make-move 
   [_ game-state]
   (->> (minmax game-state color 4)
        ((fn [[move score]] (println "making move " move " with score " score) [move score]))
        first
        (play-coin game-state))))

(defn game-loop
  [game-state current-player next-player]
  (loop [{:keys [winner] :as gs} game-state
         current-player current-player
         next-player next-player]
    (draw-board gs)
    (println gs)
    (cond
      winner (println (str "winner is " winner))
      (draw? gs) (println "game is a draw")
      :else (recur (make-move current-player gs) next-player current-player))))

#_ (game-loop (init-state 6 7) (HumanPlayer. "rohit" :red) (HumanPlayer. "parth" :yellow))
#_ (game-loop (init-state 6 7) (ComputerPlayer. "rohit" :red) (HumanPlayer. "parth" :yellow))

#_ (minmax (init-state 6 7) :red 3) 

#_  (def board {:board [[nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil] [nil nil nil nil nil :red nil] [nil nil nil :red nil :red nil] [:yellow nil :yellow :yellow :yellow :red :red]], :row-tops [5 6 5 4 5 3 5], :next-turn :yellow, :winner nil, :game-over false, :filled 9})

#_ (score-board board :red)