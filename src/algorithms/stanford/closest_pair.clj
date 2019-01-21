(ns algorithms.stanford.closest-pairs)


(declare closest-pair)

(defn get-pair
  [points]
  (closest-pair (sort-by :x points) (sort-by :y points)))

(defn euclid-distance
  [p1 p2]
  (-> (Math/pow (- (:x p1) (:x p2)) 2)
      (+ (Math/pow (- (:y p1) (:y p2)) 2))
      Math/sqrt))

(defn- closest-pair
  [px py]
  )