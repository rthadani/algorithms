(ns algorithms.graphs
  (:require [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]])
  (:import (clojure.lang PersistentQueue)))


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

(defn build-adjacency-list
  [edges]
  (reduce
   (fn [r [f t w]] (merge-with merge r {f {t w}}))
   {}
   edges))

(defn update-costs
  [g costs unvisited curr]
  (reduce-kv (fn [c neighbor neighbor-cost]
               (println "processing " neighbor)
               (if (unvisited neighbor)
                 (update c neighbor min (+ (costs curr) neighbor-cost))
                 c))
             costs
             (get g curr)))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

#_(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn remove-keys [m r]
  (reduce
   (fn [r k] (dissoc r k)) m (keys r)))

(defn dijkstra
  [edges src dst]
  (let [g (build-adjacency-list edges)]
    (clojure.pprint/pprint g)
    (loop [distances (priority-map src 0)
           result {}]
      (cond
       (empty? distances) result
       (= dst (first (peek distances))) (select-keys distances [dst])
       (empty? distances) result
       :else
       (let [[v d]         (peek distances)
             new-distances (-> (g v)
                               (remove-keys result)
                               (map-vals (partial + d)))]
         (recur (merge-with min (pop distances) new-distances)
                (assoc result v d)))))))

#_ (dijkstra [[:r :g 10] [:r :b 5] [:r :o 8]
                          [:g :r 10] [:g :b 3]
                          [:b :g 3] [:b :r 5] [:b :p 7]
                          [:p :b 7] [:p :o 2]
                          [:o :p 2] [:o :r 2]]
             :r :p)

(defn update-distances
  [edges d s]
  (reduce
   (fn [[r s] [f t d]]
     (if (= (r f) Double/POSITIVE_INFINITY)
       [r s]
       (if (< (+ d (r f)) (r t))
         [(assoc r t (+ d (r f))) (assoc s t f)]
         [r s])))
   [d s]
   edges))

(defn bellman-ford
  [edges src]
   (loop [d (assoc (reduce (fn [r [f t _]] (->
                                            (assoc r f Double/POSITIVE_INFINITY)
                                            (assoc t Double/POSITIVE_INFINITY))) {} edges) src 0)
          s (into {} (map (fn [[k _]] [k nil])d ))
          c 0]
     (if (= c (dec (count d)))
       [d (vec (map (fn [[k v]] [v k]) s))]
       (let [[d s] (update-distances edges d s)]
         (recur d s (inc c))))))

(defn has-neg-weight-cycle
  [edges d]
  (not= (first (update-distances edges d)) d))
#_ (def edges [[0 1 8] [1 3 1] [2 3 7] [0 2 1] [2 1 4]])
#_ (has-neg-weight-cycle edges (bellman-ford edges 0))


(defn build-currency-list
  [conversion-rates]
  (reduce
   (fn [g [f t r]]  (merge-with merge g {f {t r}} {t {f (/ 1 r)}}))
   {}
   conversion-rates))

(defn bfs
  [graph from-currency to-currency]
  (loop [q        (conj (PersistentQueue/EMPTY) [nil from-currency])
         explored #{from-currency}
         acc      []]
    (if (or (empty? q) (= (last acc) to-currency))
      acc
      (let [[_ current-node]   (peek q)
            adjacent-nodes (set/difference (into #{} (keys (graph current-node))) explored)]
        (recur (into (pop q) (map (fn [to] [current-node to]) adjacent-nodes))
               (into explored adjacent-nodes)
               (conj acc (peek q)))))))

(defn create-trail
  [target parents]
  (cond
    (or (nil? target) (empty? parents)) []
    (not= target (second (last parents))) (create-trail target (butlast parents))
    :else
    (conj (create-trail (first (last parents)) (butlast parents)) (second (last parents)))))

(defn convert-currency
  [currency-rates from-currency to-currency]
  (let [g (build-currency-list currency-rates)]
    (clojure.pprint/pprint g)
    (->> (bfs g from-currency to-currency)
         (create-trail to-currency)
         (partition 2 1)
         (reduce (fn [r [from to]] (println from to (get-in g [from to])) (* r (get-in g [from to]))) 1))))

#_ (convert-currency [[:USD :EUR 0.741]
         [:USD :GBP 0.657]
         [:GBP :CHF 1.614]
         [:CAD :CHF 1.049]] :USD :CAD)