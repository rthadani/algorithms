(ns algorithms.count-min-sketch
  (:require [clojure.data.priority-map :refer [priority-map]]))

(def prime Integer/MAX_VALUE)

(defn count-min-sketch
  [width hash-fns]
  {:width (int width)
   :hash-fns (into {} (for [i (range 0 (int hash-fns))
                            :let [a (rand-int prime)
                                  b (rand-int prime)]]
                [i (fn [v] (let [int-code (-> (.toString v) (.hashCode))]
                             #_(println "applying hash" a b v int-code (mod (mod (+ (*' a int-code) b) prime) width))    
                             (mod (mod (+ (*' a int-code) b) prime) (int width))))]))
   :sketch (into {} (for [i (range 0 (int hash-fns))]
              [i (vec (repeat width 0))]))})


(defn replicate-count-min-sketch
  [{:keys [sketch width] :as count-min-sketch}]
  (assoc count-min-sketch :sketch 
         (into {} (for [i (range 0 (count sketch))]
              [i (vec (repeat width 0))]))))

(defn update-count-min-sketch 
  [{:keys [hash-fns sketch] :as container} v]
  (assoc container :sketch
         (reduce
          (fn [sketch [index hash-fn]]
            (assoc sketch index (update (sketch index) (hash-fn v) inc)))
          sketch
          hash-fns)))


(defn merge-count-min-sketch
  [s1 s2]
  (assert (and 
           (= (:width s1) (:width s2))
           (= (:hash-fns s1) (:hash-fns s2))))
  (let [new-sketch (into {} (map (fn [[i j] [_ l]] [i (+ j l)]) (:sketch s1) (:sketch s2)))]
    (assoc s1 :sketch new-sketch)))

(defn frequency
  [{:keys [hash-fns sketch]} value]
  (let [indices (into {} (map (fn [[i f]]  [i (f value)]) hash-fns))]
    (->> (map (fn [[i sketch-index]] (get-in sketch [i sketch-index])) indices)
         (apply min))))


;;TODO range, perceentile
(defn top-k-sketch
  [error-rate accuracy k]
  {:count-min-sketch  (count-min-sketch (Math/ceil (/ Math/E error-rate))
                            (Math/ceil (Math/log (/ 1 (- 1 accuracy))) ))
   :k k                         
   :heap (priority-map)})

(defn update-top-k-sketch
  [{:keys [k heap count-min-sketch] :as top-k-sketch} v]
  (let [updated-sketch (update-count-min-sketch count-min-sketch v)
        frequency-v (frequency updated-sketch v)
        new-heap (assoc heap v frequency-v)
        new-heap (if (<= (count new-heap) k) new-heap (dissoc new-heap (ffirst new-heap)))]
    (-> (assoc top-k-sketch :count-min-sketch updated-sketch)
        (assoc :heap new-heap))))

(defn top-k
  [{:keys [heap]}]
  (map first (rseq heap)))



(-> (count-min-sketch 6.0 4.0)
    (update-count-min-sketch  "A")
    (update-count-min-sketch "B")
    (update-count-min-sketch "C")
    (update-count-min-sketch "A")
    (frequency "A"))

(-> (top-k-sketch 0.0001 0.95 2)
  (update-top-k-sketch  "A")
  (update-top-k-sketch "B")
  (update-top-k-sketch "C")
  (update-top-k-sketch "A")
  (update-top-k-sketch "A")
  (update-top-k-sketch "D")
  (update-top-k-sketch "C")
  (top-k))
