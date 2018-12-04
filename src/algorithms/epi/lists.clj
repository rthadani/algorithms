(ns algorithms.epi.lists)


(defrecord ConsCell [v r])

(defn add-element
  [c e]
  (if c
    (ConsCell. (:v c) (add-element (:r c) e))
    (ConsCell. e nil)))

(defn new-list
  [& args]
  (reduce (fn [l e] (add-element l e)) nil args))

(defn remove-last
  ([list]
   (remove-last list (:r list)))
  ([c n]
   (println c n)
   (if (nil? (:r n))
     (ConsCell. (:v c) nil)
     (ConsCell. (:v c) (remove-last (:r c) (:r (:r c)))))))

(defn to-string
  [list]
  (loop [list list
         result ""]
    (if list
      (recur (:r list) (str result " " (:v list)))
      result)))


(defn add-head
  [list val]
  (ConsCell. val list))

(defn head-list
  [list]
  (:v list))

(defn last-list
  [list]
  (if (:r list)
    (last-list (:r list))
    list))

(defn nth-node
  [list n]
  (if (zero? n)
    list
    (nth-node (:r list) (dec n))))

(defn rest-list
  [list]
  (:r list))

(defn empty-list?
  [list]
  (or (nil? list) (nil? (:v list))))


(defn length-list
  [l]
  (if (empty-list? l)
    0
    (inc (length-list (rest-list l)))))

(defn add-last
  [c cell]
  (if (nil? (:r c))
    (ConsCell. (:v c) cell)
    (ConsCell. (:v c) (add-last (:r c) cell))))


(length-list (new-list 1 2 3 4 5))
(to-string (add-element (new-list 1 2 3 4) 5))
(to-string (remove-last (new-list 1 2 3 4)))
(nth-node (new-list 1 2 3) 1)

(defn merge-sorted
  [l1 l2]
  (cond
    (empty? l1) l2
    (empty? l2) l1
    (< (head-list l1) (head-list l2))
    (add-head (merge-sorted (rest-list l1) l2) (head-list l1))
    :else
    (add-head (merge-sorted l1 (rest-list l2)) (head-list l2))))
(to-string (merge-sorted (new-list 1 4 5) (new-list 2 6)))

(defn reverse-list
  [l]
  (if (empty-list? l)
    l
    (let [h (head-list l)
          r (reverse-list (rest-list l))]
      (add-element r  h))))
(to-string (reverse-list (new-list 1 2 3)))


(defn reverse-sublist
  [l start end]
  (letfn [(get-sublist [l start end k]
                        (cond
                          (< k start) (get-sublist (:r l) start end (inc k))
                          (= k end) (ConsCell. (:v l) nil)
                          (>= k start) (ConsCell. (:v l) (get-sublist (:r l) start end (inc k)))))
          (set-sublist-at-node [sublist start]
                               (println "sublist " sublist)
                               (cond
                                 (> start 0) (ConsCell. (:v l) (set-sublist-at-node (:r sublist) (dec start)))
                                 (zero? start) (ConsCell. (:v sublist) (add-last (:r sublist) (nth-node l (+ start (- end start)))))))]
    (-> (get-sublist l start end 0)
        (reverse-list)
      (set-sublist-at-node start))))
(to-string (reverse-sublist (new-list 1 2 3 4 5 6) 1 3))

(defn even-odd-merge
  [l]
  (let [count (length-list l)]
    (loop [list l
           even-list nil 
           odd-list nil
           i 0]
      (cond
        (= i count) (add-last even-list odd-list)
        (zero? (mod i 2)) (recur (:r list) 
                            (add-element even-list (:v list)) 
                            odd-list 
                            (inc i))
        :else (recur (:r list) 
                     even-list 
                     (add-element odd-list (:v list)) 
                     (inc i))))))

(to-string(even-odd-merge (new-list 1 2 3 4 5 6)))       

(defn add-two-numbers
  ([list1 list2]
   (add-two-numbers (cons 0 list1) (cons 0 list2) 0 '()))
  ([list1 list2 carry soln]
   (if (and (empty-list? list1) (empty? list2))
     (let [r (reverse soln)]
       (if (zero? (first r)) (rest r) r))
     (let [[first1 & rest1] list1
           [first2 & rest2] list2
           sum (+ (if (nil? first1) 0 first1) (if (nil? first2) 0 first2) carry)]
       (add-two-numbers rest1 rest2 (int (/ sum 10)) (cons (mod sum 10) soln))))))

(add-two-numbers '(2 4 3) '(5 6 4))



;;can only do by value


#_(defn make-cycle-list
    [length]
    (let [cycle-at (rand-int length)
          new-list (apply new-list (range 0 length))
          cycle-node (nth-node new-list cycle-at)]
      (println cycle-at cycle-node)
      (replace-last new-list cycle-node)))

#_(make-cycle-list 4)

(defn flatten-list
  [list]
  (println list)
  (cond 
    (empty? list) nil
    (seq? (first list)) (concat (flatten-list (first list))(flatten-list (rest list))) 
    :else (cons (first list) (flatten-list (rest list)))                        
    ))
(flatten-list (list 1 (list 2 3) 4))