(ns algorithms.count-min-sketch)

(def prime Integer/MAX_VALUE)
(defn init
  [width hash-fns]
  {:width width
   :hash-fns (into {} (for [i (range 0 hash-fns)
                            :let [a (rand-int prime)
                                  b (rand-int prime)]]
                [i (fn [v] (let [int-code (-> (.toString v) (.hashCode))]
                             #_(println "applying hash" a b v int-code (mod (mod (+ (*' a int-code) b) prime) width))    
                             (mod (mod (+ (*' a int-code) b) prime) width)))]))
   :sketch (into {} (for [i (range 0 hash-fns)]
              [i (vec (repeat width 0))]))})

(defn serialize-sketch
  [sketch]) ;TODO

(defn update-sketch 
  [{:keys [hash-fns sketch] :as container} v]
  (assoc container :sketch
         (reduce
          (fn [sketch [index hash-fn]]
            (assoc sketch index (update (sketch index) (hash-fn v) inc)))
          sketch
          hash-fns)))



(defn frequency
  [{:keys [hash-fns sketch]} value]
  (let [indices (into {} (map (fn [[i f]]  [i (f value)]) hash-fns))]
    (->> (map (fn [[i sketch-index]] (get-in sketch [i sketch-index])) indices)
         (apply min))))


;;TODO range top k hitters, perceentile
(defn rand-str [len]
  (apply str (take len (repeatedly #(char (+ (rand 26) 65))))))

(def sketch (init 16 16))

(-> (update-sketch (init 6 4) "A")
    (update-sketch "B")
    (update-sketch "C")
    (update-sketch "A")
    (frequency "A"))

(def applied-sketch  (let [random-chars (take 26 (repeatedly #(rand-str 4)))]
                       (println random-chars)
                       (reduce (fn [sketch c] (update-sketch sketch c)) sketch random-chars))) 

(frequency applied-sketch "VJUD")

