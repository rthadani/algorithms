(ns algorithms.stanford.karatsuba)

(defn fast-multiply 
  [n1 n2]
  (if (or (< n1 100) (< n2 100))
    (* n1 n2)
    (let [m (Math/floor (/ (min (Math/log10 n1) (Math/log10 n2)) 2))
          split (Math/pow 10 m)
          hi1 (bigint (/ n1 split))
          lo1 (bigint (mod n1 split))
          hi2 (bigint (/ n2 split))
          lo2 (bigint (mod n2 split))
          z0 (fast-multiply lo1 lo2)
          z1 (fast-multiply (+ lo1 hi1) (+ lo2 hi2))
          z2 (fast-multiply hi1 hi2)]
      (-> (* z2 (Math/pow 10 (* m 2)))
        (+ (* (- z1 z2 z0) (Math/pow 10 m)))
          (+ z0)))))

#_ (fast-multiply 241 241)
#_ (fast-multiply 3141592653589793238462643383279502884197169399375105820974944592 2718281828459045235360287471352662497757247093699959574966967627)