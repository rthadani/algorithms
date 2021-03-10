(ns algorithms.knn)


(defn cosine-distance
  [v1 v2]
  (let [numerator (apply + (map (fn [v1 v2] (* v1 v2)) v1 v2))
        denominator1 (apply + (map #(* % %) v1))
        denominator2 (apply + (map #(* % %) v2)) ]
    (/ numerator (* (Math/sqrt denominator1) (Math/sqrt denominator2)))))

(defn knn-classifier 
  [unknown knownvs labels distance-fn k]
  (let [all-distances (map-indexed  (fn [i it]  [(distance-fn unknown it) i]) knownvs)
        k-nearest (->> (sort-by first all-distances) (take k))
        k-nearest-labels (map (fn [[_ i]]  (get labels i)) k-nearest)]
    (->> (frequencies k-nearest-labels)
         (sort-by second)
         ffirst)))

(def train
  [[1.70, 65, 20]
   [1.90, 85, 33]
   [1.78, 76, 31]
   [1.73, 74, 24]
   [1.81, 75, 35]
   [1.73, 70, 75]
   [1.80, 71, 63]
   [1.75, 69, 25]])

(def train-labels
  [:programmer
   :builder
   :builder
   :programmer
   :builder
   :scientist
   :scientist
   :programmer])

#_ (knn-classifier [1.74 67 22] train train-labels cosine-distance 2)

