(ns interview.connect4)

(defn init-state
  [player1-id player2-id]
  (let [board (vec (for [_ (range 0 6)]
                 (vec (repeat 7 nil))))
        row-tops (vec (repeat 7 6))]
    {:board board
     :row-tops row-tops
     :player-1 player1-id
     :player-2 player2-id
     :next-turn :player-1
     :winner nil
     :filled 0}))

(defn draw-board
  [{:keys [board player-1 player-2]}]
  (doseq [i (range 0 (count board))
          j (range 0 (count (board 0)))
          :let [slot (get-in board [i j])]]
    (cond 
      (nil? slot) (print "_ ")
      (= player-1 slot) (print "X ")
      (= player-2 slot) (print "O "))
    (when (= j (dec (count (board 0))))
      (println ""))))

;;win functions
;;;;extract rows columns and diagonals of 4 to check if there is a winner
(defn extract-horizontal
  [board row col]
  (for [i (range -3 4)
        :let [s (+ i col)
              e (+ 3 s)]
        :when (and (>= s 0) (< e (count (board 0))))]
    (subvec (board row) s (inc e))))

(defn extract-vertical
  [board row col]
  (for [i (range -3 4)
        :let [s (+ i row)
              e (+ 3 s)]
        :when (and (>= s 0) (< e (count board)))]
    (vec (for [j (range 0 4)]
           (get-in board [(+ s j) col])))))

(defn extract-desc-diagonal
  [board row col]
  (for [i (range -3 4)
        :let [start-row (+ i row)
              end-row (+ 3 start-row)
              start-col (+ i col)
              end-col (+ 3 start-col)]
        :when (and (>= start-row 0) (< end-row (count board)) (>= start-col 0) (< end-col (count (board 0))))]
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
        :when (and (>= start-row 0) (< end-row (count board)) (>= start-col 0) (< end-col (count (board 0))))]
    (vec (for [j (range 0 4)]
           (get-in board [(+ start-row j) (- start-col j)])))))

(defn extract-run
  [board row col]
  (let [h      (extract-horizontal board row col)
        v      (extract-vertical board row  col)
        dd     (extract-desc-diagonal board row  col)
        da     (extract-asc-diagonal board row col)]
    (concat h v dd da)))


(defn winner?
  [{:keys [board row-tops]} col player-id]
  (let [win-fn (fn [r] (every? #(= player-id %) r))
        runs (extract-run board (row-tops col) col)]
    (some #(win-fn %) runs)))

;; all code to  check winner ends here

(defn play-in-slot 
  [{:keys [next-turn row-tops] :as gs} col]
  ;; todo validate input here 
  (let [row-top (dec (row-tops col))
        game-state (-> (update gs :board #(assoc-in % [row-top col] next-turn))
                       (update :row-tops #(assoc % col row-top))
                       (update :next-turn #(if (= % :player-1) :player-2 :player-1))
                       (update :filled inc))]
    (if 
     (winner? game-state col next-turn)
      (assoc game-state :winner next-turn)
      game-state)))

(defprotocol Player
  (make-move [this game-state]))

(defrecord PrimitiveAI [id]
  Player
  (make-move [_ {:keys [row-tops board] :as gs}]
             (let [available (filter #(not= (second %) 0) (map-indexed (fn [i e] [i e] ) row-tops))
                   next-col (if (empty? available)
                              (println "this is not good") ;; throw exception here if needed
                              (first (nth available (rand-int (count available)))))]
               (play-in-slot gs next-col))))


(defn minmax
  [{:keys [next-turn winner row-tops] :as gs} color depth]
  (cond
    winner (if (= winner next-turn) [nil 1] [nil -1])
    (draw? gs) [nil 0]
    :else (if (zero? depth)
            [nil (score-board gs next-turn)]
            (->> (for [col row-tops
                       :when (> (row-tops col) 0)
                       :let [[_ score] (minmax (play-in-slot gs col) color (dec depth))]]
                   [col (- score)])
                 (apply max-key second)))))


(defn game-loop
  [game-state current-player next-player]
  (loop [{:keys [winner filled] :as gs} game-state
         current-player current-player
         next-player next-player]
    (draw-board gs)
    (cond 
      winner (println "winner is " winner)
      (= filled 42) (println "game is a draw")
      :else (recur (make-move current-player gs) next-player current-player))))
(let 
 [player-1 (PrimitiveAI. 1)
  player-2 (PrimitiveAI. 2)
  game-state (init-state (:id player-1) (:id player-2))]
  (game-loop game-state player-1 player-2))
#_ (draw-board (->> (make-move player-1 game-state)
                    (make-move player-2)))