(ns algorithms.hll)

(defn trailing-zeroes
  [num]
  (loop [n num
         count 0]
    (cond (zero? n) 32
          (zero? (bit-and n 1)) (recur (bit-shift-right n 1) (inc count))
          :else  count)))

(defn update-bucket
  [k buckets value]
  (let [hash (bit-and (.hashCode (str value)) 0x7fffffff)
        bucket (bit-and hash (dec (count buckets)))
        bucket-hash (bit-shift-right hash k)]
        #_(println hash bucket bucket-hash buckets (buckets bucket) (trailing-zeroes bucket-hash))
    (assoc buckets bucket (max (buckets bucket) (trailing-zeroes bucket-hash)))))

(defn cardinality 
  [k values]
  (let [buckets (Math/pow 2 k)
        max-zeroes (vec (repeat buckets 0))
        max-zeroes (reduce (partial update-bucket k) max-zeroes values)]
    (-> (Math/pow 2 (/ (apply + max-zeroes) buckets))
        (* buckets 0.79402)) ))

 (->> (repeatedly 100000 rand)
        #_(map (fn [n] (bit-and (.hashCode (str n)) 0x7fffffff)))
        (cardinality 10))