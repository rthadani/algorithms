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


;;topological sort
;;assumes agraph with no cycles
(defn in-degree
  [graph]
  (reduce
    (fn [degree [node outgoing]]
      (as-> (update degree node (fnil identity 0)) $
            (reduce (fn [degree node] (update degree node (fnil inc 0))) $ outgoing)))
    {}
    graph))

(defn decrease-in-degree
  [graph node in-degree]
  (reduce
    (fn [in-degree node]
      (update in-degree node dec))
    in-degree
    (get graph node)))

(defn get-new-root-nodes
  [in-degree]
  (keys (filter (fn [[_ degree]] (zero? degree)) in-degree)))

(defn filter-0-indegree
  [in-degree]
  (into {} (filter (fn [[_ degree]] (not (zero? degree))) in-degree)))

(defn topological-sort
  [graph]
  (let [in-degree (in-degree graph)
        queue     (into (PersistentQueue/EMPTY) (get-new-root-nodes in-degree))]
    (loop [q   queue
           i   (filter-0-indegree in-degree)
           acc []]
      (if (empty? q)
        acc
        (let [node          (peek q)
              new-in-degree (decrease-in-degree graph node i)
              new-roots     (get-new-root-nodes new-in-degree)]
          (recur (into (pop q) new-roots)
                 (filter-0-indegree new-in-degree)
                 (conj acc (peek q))))))))

#_(topological-sort {:5 [:2 :0]
                     :2 [:3]
                     :4 [:0 :1]
                     :3 [:1]
                     :0 []})