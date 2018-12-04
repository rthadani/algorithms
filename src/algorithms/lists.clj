(ns algorithms.lists)

(defn is-palindrome [x]
  (cond
    (empty? x) true
    (= 1 (count x)) true
    (and (= 2 (count x)) (= (first x) (last x))) true
    :else (and (= (first x) (last x)) (is-palindrome (drop-last (rest x))))))

(defn detect-cycle
  [special-list]
  (loop [slow (:next special-list)
         fast (get-in special-list [:next :next])]
    (cond
      (nil? fast) false
      (= fast slow) true
      :else (recur (:next special-list) (get-in special-list [:next :next])))))
