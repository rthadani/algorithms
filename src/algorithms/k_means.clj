(ns algorithms.k-means)

(defn init-k-means
  [k iterations distance-fn]
  {:groups k
   :iterations iterations
   :distance-fn distance-fn})

(defn cluster-for-point
  [point cluster-means distance-fn]
  (->> (map-indexed (fn [i v] [i (distance-fn point v)]) cluster-means)
      (apply min-key second)
      first))

(defn make-clusters
  [points cluster-means distance-fn]
  (reduce
   (fn [clusters point] (as-> (cluster-for-point point cluster-means distance-fn) $
                             (conj (clusters $) point)))
   (vec (repeat (count cluster-means) []))
   points))

(defn average [coll] 
  (/ (reduce + coll) (count coll)))

(defn cluster-means
  [clusters]
  (map average clusters))

(defn k-means
  [points {:keys [groups iterations distance-fn]}]
  (loop [c (vec (repeat (count groups) []))
         cms (vec (repeat groups (rand-nth points)))
         i 0]
    (if (= iterations i) 
      [c cms]
      (let [new-clusters         (make-clusters points cluster-means distance-fn)
            indexed-new-clusters (map-indexed (fn [i v] [i v]) new-clusters)]
        (if (every? (fn [[i new-c]]  (= (count (new-c i)) (count c))) indexed-new-clusters)
          [(cluster-means new-clusters) new-clusters]
          (recur new-clusters (cluster-means new-clusters) (inc i)))))))