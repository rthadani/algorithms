(ns algorithms.epi.misc)

(defn researcher-productivity
  [published]
  (->>  published
        (into (sorted-map-by  (fn [k1 k2] (compare [(get published k1) k1] [(get published k2) k2]))))
        (into [])
        (reduce
         (fn [prod [p c]]
           (if (< (- (count published) c) 0)
             (reduced prod)
             c)))))
;Sort the  papers by citations if there more more papers available than the current citation count then keep looking 
(researcher-productivity {\A 2, \B 1, \C 4})

(defn count-bits[i]
  (loop [current i
         bits 0]
    (cond
      (zero? current) bits
      (= 1 (bit-and current 1))(recur (bit-shift-right current 1) (inc bits))
      :else (recur (bit-shift-right current 1) bits))))

(count-bits 2)

(bit-shift-right 2 1)

