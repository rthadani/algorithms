(ns algorithms.graphs
  (:require [clojure.set :as set]))


(defn make-graph
  [input]
  (reduce
   (fn [g [f t]] (merge-with concat {t []} {f []} g {f [t]}))
    {} 
   input))

(defn visit-counts
  [g]
   (->> (vals g)
        (reduce concat [])
        (group-by identity)
        (map (fn [[k v]] [k (count v)]))
        (into {})
        (merge-with (fn [x y] (if y (inc y) x)) {"JFK" 1})))


(defn get-possible-destinations
  [g from visit-counts]
  (sort 
   (filter #(> (get visit-counts %) 0)  (get g from))))

(defn find-iterenary*
  [current-airport so-far visit-counts graph]
  (println current-airport so-far)
  (if (every? zero? (vals visit-counts))
    [so-far]
    (for [next-location (get-possible-destinations graph current-airport visit-counts)
          :let [new-visit-counts (update visit-counts next-location dec)]
          solution (find-iterenary* next-location (conj so-far next-location) new-visit-counts graph)]
      solution)))

(defn find-iterenary
  [tickets]
  (let [g (make-graph tickets)
        v (update (visit-counts g) "JFK"  dec) ]
       (first (find-iterenary* "JFK" ["JFK"] v g))))

#_ (find-iterenary [["MUC", "LHR"], ["JFK", "MUC"], ["SFO", "SJC"], ["LHR", "SFO"]])
#_ (visit-counts (make-graph [["MUC", "LHR"], ["JFK", "MUC"], ["SFO", "SJC"], ["LHR", "SFO"]]))
#_ (find-iterenary [["JFK","SFO"],["JFK","ATL"],["SFO","ATL"],["ATL","JFK"],["ATL","SFO"]])