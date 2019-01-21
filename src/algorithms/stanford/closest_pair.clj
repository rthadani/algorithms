(ns algorithms.stanford.closest-pairs)


(declare closest-pair)

(defn get-pair
  [points]
  (closest-pair (sort-by :x points)))

(defn euclid-distance
  [p1 p2]
  (-> (Math/pow (- (:x p1) (:x p2)) 2)
      (+ (Math/pow (- (:y p1) (:y p2)) 2))
      Math/sqrt))

(defn brute-force
  [px]
  (min (for [i (range 0 (count px))
             j (range i (count px))]
         (euclid-distance (px i) (px j)))))

(defn- mid-strip
  [px delta]
  (let [mid (int (/ (count px) 2))
        mid-x (:x (px mid))  ]
    (vec (filter (fn [{:keys [x]}] (< delta (Math/abs (- x mid-x)) )) px))))

(defn- min-instrip-distance
  [strip delta]
  (let [sy (vec (sort-by :y strip))
        min-distance (for [i (range 0 (count sy))
                           j (range i (count sy))
                           :let [{:keys [yi]} (sy i)
                                 {:keys [yj]} (sy j)
                                 dist (Math/abs (- yi yj))]
                           :when (< dist delta)]
                       dist)]
    (if (empty? min-distance)
      Integer/MAX_VALUE
      (apply min min-distance))))


(defn- closest-pair
  [px]
  (if (<= (count px) 3) 
    (brute-force px)
    (let [mid (int (/ (count px) 2))
          dl (closest-pair (subvec px 0 mid))
          dr (closest-pair (subvec px mid (count px)))
          split-strip (mid-strip px (min dl dr))]
      (min dl dr (min-instrip-distance split-strip (min dl dr))))))