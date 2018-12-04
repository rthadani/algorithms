(ns algorithms.epi.stack-queue)

;;stack with max
(defn push [stack e]
  (let [{:keys [m]} (first stack)]
       (if (or (nil? m) (> e m))
         (cons {:e e :m e} stack)
         (cons {:e e :m m} stack))))
(defn pop 
  [stack]
  (let [{:keys [m e]} (first stack)]
       [e (rest stack)]))

(defn max-stack
  [stack]
  (:m (first stack)))

(defn peek
  [stack]
  (:e (first stack)))

(def s (list))
(def stack (-> (push s 1)
               (push 5)
               (push 6)
               (push 3)
               (push 4)))
stack            


(max-stack stack)

(-> (pop stack)
    second
    (max-stack))

(-> (pop stack)
 second
 (pop)
 second
 (max-stack))

(+ 2 2)


(defn new-node
  [val]
  {:l nil :r nil :v val})

(def new-tree
  (-> (new-node 314)
      (assoc :l (new-node 6))
      (assoc :r (new-node 6))
      (assoc-in [:l :l] (new-node 271))
      (assoc-in [:l :r] (new-node 561))
      (assoc-in [:r :l] (new-node 2))
      (assoc-in [:r :r] (new-node 271))
      (assoc-in [:l :l :l] (new-node 28))
      (assoc-in [:l :l :r] (new-node 0))
      (assoc-in [:l :r :r] (new-node 3))
      (assoc-in [:r :l :r] (new-node 1))
      (assoc-in [:r :r :r] (new-node 28))
      (assoc-in [:l :r :r :l] (new-node 17))
      (assoc-in [:r :l :r :l] (new-node 401))
      (assoc-in [:r :l :r :r] (new-node 257))
      (assoc-in [:r :l :r :l :r] (new-node 641))))
      
#_ new-tree

;;depth ordr traversal
(defn depth-traversal
  [root]
  (loop [q (into (clojure.lang.PersistentQueue/EMPTY) root)
         result [(:v root)]]
    (println "here")
    (if (zero? (count q))
      result
      (let [elements (mapcat (fn [e] (println "mapcat" e) [(into {} (:l e)) (into {} (:r e))]) q)]
        (recur (into (clojure.lang.PersistentQueue/EMPTY) (filter #(not (nil? %)) elements))
               (into [] (concat result (map :v elements))))))))

#_  (depth-traversal (new-node 314))

;;building wiht views
(defn sunset-buildings [& l]
  (letfn [(pop-until-taller [stack height]
            (if (or (empty? stack) (> (first stack) height))
              stack
              (pop-until-taller (rest stack) height)))]
         (reduce
          (fn [result b] 
            (-> result
             (pop-until-taller b)
             (conj b) ))
          '()
          l)))

#_ (sunset-buildings 1 4 2 3)

(defn enque-stack-queue
  [queue val]
  (let [[e d] queue]
       [(cons val e) d]))
(defn dequeue-stack-queue
  [[e d]]
  (letfn [(move-from-e-to-d [e d]
                            (if (empty? e)
                              [e d]
                          (move-from-e-to-d (rest e) (cons (first e) d))))]
    (if (empty? d) 
      (let [[e d] (move-from-e-to-d e d)]
        [[e (rest d)] (first d)])
      [[e (rest d)] (first d)])))