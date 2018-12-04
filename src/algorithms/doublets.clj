(ns algorithms.doublets
  (:require [clojure.data :as data]))

(defn word-seperated-by-a-letter?
  [source target]
  (let [[first second _] (data/diff (seq source) (seq target))]
    (= 1 (count (filter some? first)) (count (filter some? second)))))

(defn prepare-dictionary
  [dictionary]
  (->> (for [source dictionary
             target dictionary
             :when (word-seperated-by-a-letter? source target)]
         [source target])

       (group-by first)
       (reduce (fn [acc [key vals]]
                 (->> (apply concat vals)
                      (filter #(not= key %))
                      distinct
                      (assoc acc key)))
               {})))

(defn make-tree* [start dictionary seen]
  [start (for [child (get dictionary start)
               :when (not-any? #(= child %) @seen)
               :let [_ (swap! seen conj child)]]
           (make-tree* child dictionary seen))])

(defn make-tree [start dictionary]
  (let [seen (atom [start])]
    (make-tree* start dictionary seen)))

(def words (-> "words.edn"
               (clojure.java.io/resource)
               (slurp)
               (read-string)))

(def dictionary (prepare-dictionary words))

(defn find-path
  [tree dest solution]
  (cond
    (= dest (first tree)) (conj solution (first tree))
    (empty? tree) nil
    :else
      (let [children (fnext tree)
            solution (conj solution (first tree))
            solve-for-child (find-path (first children) dest solution)]
        (if-not solve-for-child
          (find-path (fnext children) dest solution)
          solve-for-child))))

(defn doublet [source dest]
  (-> (make-tree source dictionary)
      (find-path dest [])
      (clojure.pprint/pprint)))

(doublet "door" "boom")
(doublet "wheat" "bread")
(doublet "bank" "loan")
