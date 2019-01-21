(ns algorithms.primes)

(defn sieve [s]
  (cons (first s)
        (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                 (rest s))))))

#_ (take 1000 (sieve (iterate inc 2)))
(defn range-primes 
  [start end]
  (->>
   (take-while #(<= % end) (sieve (iterate inc 2)))
   (filter #(>= % start))))
#_ (range-primes 9000 100000)
