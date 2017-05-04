(ns algorithms.geeksforgeeks.dynamic)

;;longest increasing subsequence
;;Opt(i) = 1 + max(Opt(j)) if elem(i) > elem(j) and j < i)
;;       = 1 otherwise
(defn lis
  [array]
  (if (empty? array)
    [0 []]
    (let [elem (last array)
       lisi-1 ]
      )))
