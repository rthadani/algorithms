(ns algorithms.geeksforgeeks.graph
  (:require [clojure.set :as set])
  (:import (clojure.lang PersistentQueue)))

;;graphs represented as adjacency lists
;;{:key [adjacent nodes]}
(defn bfs
  [graph start-node]
  (loop [q        (conj (PersistentQueue/EMPTY) start-node)
         explored #{start-node}
         acc      []]
    (if (empty? q)
      acc
      (let [current-node   (peek q)
            adjacent-nodes (set/difference (into #{} (get graph current-node)) explored)]
        (recur (into (pop q) adjacent-nodes)
               (into explored adjacent-nodes)
               (conj acc current-node))))))
#_(bfs {:1 [:2 :3]
        :2 [:4]
        :3 [:4]
        :4 [:1]} :1)

(defn dfs
  [graph start-node]
  (letfn [(dfs-internal
            [node explored]
            (if (contains? explored node)
              explored
              (let [neighbors (filter (fn [n] (not (contains? (into #{} explored) n))) (get graph node))]
                (reduce (fn [acc v] (dfs-internal v acc)) (conj explored node) neighbors))))]
    (dfs-internal start-node [])))
#_(dfs {:1 [:2 :3]
        :2 [:4]
        :3 [:4]
        :4 [:1]} :1)
