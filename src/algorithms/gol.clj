(ns algorithms.gol
  (:require [clojure.core.match :refer [match]]
            [clojure.core.async :refer [go-loop >!!]]
            [clojure.set :as set]))

(def neighbor-offsets
  (for [x (range -1 2)
        y (range -1 2)
        :let [offset [x y]]
        :when (not= offset [0 0])]
    offset))

(defn neighbors
  [cell]
  (into #{} (map #(mapv + % cell) neighbor-offsets)))

(defn live-neighbors
  [cell current-state]
  (->> (neighbors cell)
       (set/intersection current-state)))

(defn dead-neighbors
  [cell current-state]
  (->> (neighbors cell)
       (filter #(not (contains? current-state %)))))

(defn survivors
  [current-state]
  (into #{}
        (for [cell current-state
              :let [alive (count (live-neighbors cell current-state))]
              :when (or (= alive 2) (= alive 3))]
          cell)))

(defn offspring
  [current-state]
  (into #{}
        (for [cell current-state
              child (dead-neighbors cell current-state)
              :when (= 3 (count (live-neighbors child current-state)))]
          child)))

(defn tick
  [current-state]
  (set/union (survivors current-state) (offspring current-state)))

(defn game-of-life
  [initial-state state-channel]
  (go-loop [state initial-state]
    (>!! state-channel state)
    (recur (tick state))))

(def glider #{[0 1] [1 2] [2 0] [2 1] [2 2]})
(def gosper-glider-gun #{[1 5] [2 5] [1 6] [2 6]
                         [11 5] [11 6] [11 7] [12 4] [12 8] [13 3] [13 9] [14 3] [14 9]
                         [15 6] [16 4] [16 8] [17 5] [17 6] [17 7] [18 6]
                         [21 3] [21 4] [21 5] [22 3] [22 4] [22 5] [23 2] [23 6] [25 1] [25 2] [25 6] [25 7]
                         [35 3] [35 4] [36 3] [36 4]})
