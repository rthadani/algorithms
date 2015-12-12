(ns algorithms.advent-of-code)

;;1
(defn not-quite-lisp
  [input break-when]
  (reduce (fn [acc paren]
            (let [new-acc (case paren
                            \( [(inc (first acc)) (inc (second acc))]
                            \) [(dec (first acc)) (inc (second acc))])]
              (if (break-when (first acc))
                acc
                new-acc)))
          [0 0]
          input))

(defn part2
  [[floor pos]]
  (= floor -1))
#_(not-quite-lisp msg (constantly false))
#_(not-quite-lisp msg #(= % -1))

;;2
(defn wrapping-size [l w h]
  (+ (* 2 l w) (* 2 w h) (* 2 h l) (min (* l w) (* w h) (* h l))))

(defn convert-dims [str]
  (->> (clojure.string/split-lines str)
       (map #(clojure.string/split % #"x"))
       (map #(map (fn [x] (Integer. x)) %))))

(defn wrapping-paper [input]
  (->> (convert-dims input)
       (map #(apply wrapping-size %))
       (apply +)))

;;3
(defn santa-delivery
  [directions]
  (->> (reduce (fn [acc dir]
                 (let [[x y] (last acc)]
                   (case dir
                        \> (conj acc [(inc x) y])
                        \< (conj acc [(dec x) y])
                        \^ (conj acc [x (inc y)])
                        \v (conj acc [x (dec y)]))))
               [[0 0]]
               directions)
       (into #{})))
#_ (count (santa-delivery msg))

(defn santa+robo-delivery
  [directions]
  (clojure.set/union
    (-> (take-nth 2 directions)
        santa-delivery)
    (-> (take-nth 2 (rest directions))
        santa-delivery)))

#_ (count (santa+robo-delivery msg))
