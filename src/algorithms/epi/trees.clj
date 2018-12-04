(ns algorithms.epi.trees)

(defn new-node
  [val]
  {:v val :l nil :r nil})


(defn create-tree
  ([nodes]
   (create-tree nodes (atom 0)))
  ([nodes skip-nodes]
   (let [current-node @skip-nodes
         root (if (>= current-node (count nodes)) nil (nth nodes current-node)) ]
     (swap! skip-nodes inc)
     (if (nil? root)
       root
       (-> (new-node root)
         (assoc :l (create-tree nodes skip-nodes))
         (assoc :r (create-tree nodes skip-nodes)))))))

(def tree (create-tree [1 2 nil nil 3 4 nil nil 5 nil nil]))

(defn height
  [{:keys [l r]}]
  (if (and (nil? l) (nil? r))
    0
    (inc (max (height l) (height r)))))
  (height tree)

(defn is-balanced?
  [{:keys [l r]}]
  (if (and (nil? l) (nil? r))
    true
    (and (is-balanced? l) (is-balanced? r)
         (<= (Math/abs (- (height l) (height r))) 1))))
(is-balanced? tree)

(defn is-symmetric?
  ([{:keys [l r] :as tree}]
   (or (nil? tree) (is-symmetric? l r)))
  ([l r]
   (or (every? nil? [l r])
       (and (= (:v l) (:v r))
            (is-symmetric? (:l l) (:r r))
            (is-symmetric? (:r l) (:l r))))))

(def symmetric-tree (create-tree [314 6 nil 2 nil 3 nil nil 6 2 3 nil nil nil nil]))  
#_ (is-symmetric? symmetric-tree)
#_ (is-symmetric? tree)

;;least common accencstor
(defn lca-by-value
  [tree v1 v2]
   (letfn [(find-node [t v] 
                      (cond  
                        (= v (:v t) ) true
                        (nil? t) false
                        :else (or (find-node (:l t)  v) (find-node (:r t) v))))]
     (let [v1-on-left (find-node (:l tree) v1)
          v2-on-left (find-node (:l tree) v2)]
              (println (:v tree) v1-on-left v2-on-left)
               (cond 
                 (not= v1-on-left v2-on-left) (:v tree)
                 v1-on-left (lca-by-value (:l tree) v1 v2)
                 :else (lca-by-value (:r tree) v1 v2)))))

#_ (lca-by-value tree 2 4)

(defn sum-root-to-leaf-path
  ([binary-bit-tree]
   (sum-root-to-leaf-path binary-bit-tree 0))
  ([{:keys [l r v] :as tree} sum-so-far]
   (cond
     (not tree) 0
     (every? nil? [l r]) sum-so-far
     :else (let [sum-so-far (+ v (* 2 sum-so-far))]
                (+ (sum-root-to-leaf-path l sum-so-far) (sum-root-to-leaf-path r sum-so-far))))))
(def binary-bit-tree (create-tree [1 0 0 0 nil nil 1 nil nil 1 nil 1 0 nil nil nil 1 0 nil 0 1 nil 1 0 nil nil 0 nil 0 nil nil]))
(sum-root-to-leaf-path binary-bit-tree)
  

(defn inorder-no-stack
  [tree]
  (loop [stack (cons tree nil)
         result []]
    (let [current-node (first stack)]
      (println stack result current-node)
      (cond
        (empty? stack) result
        (:l current-node) (recur (cons (:l current-node) stack) result) 
        (nil? (:l current-node))
          (recur
          (if (:r current-node) 
            (cons (:r current-node) (rest stack)) 
            (rest stack))
          (conj result (:v current-node)))
        (not (:r current-node)) (recur (rest stack) result)
        :else (recur (cons (:r current-node) stack) result))) ))
#_ (inorder-no-stack tree)

(defn dfs
  [{:keys [l r v] :as tree}]
  (if (nil? tree) 
    []
    (concat (dfs l) (dfs r) [v])))
#_ (dfs tree)  

(defn bfs
  [tree]
  (loop [q (into (clojure.lang.PersistentQueue/EMPTY) [tree])
         result []]
        (if (seq q)
          (let [node (peek q)]
            (recur (into (pop q) (filter identity [(:l node) (:r node)]))
                   (conj result (:v node))))
          result))) 
#_ (bfs tree)


