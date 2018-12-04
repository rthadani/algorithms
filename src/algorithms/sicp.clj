(ns algorithms.sicp)

(defn- good-enough? [x guess]
  (< (-> (* guess guess)
         (- x)
         (Math/abs))
     0.001))

(defn- avg [& params]
  (/ (apply + params) (count params)))

(defn improve [x guess]
  (avg guess (/ x guess)))

(defn- sqrt
  ([x]
   (sqrt x 1.0))
  ([x guess]
   (if (good-enough? x guess)
     guess
     (recur x (improve x guess)))))

(defn- better-good-enough? [x current-guess prev-guess]
  (or (< (Math/abs (- current-guess prev-guess)) 0.001)
      (good-enough? x current-guess)))

(defn- better-sqrt
  ([x]
   (better-sqrt x 1.0 Integer/MAX_VALUE))
  ([x current-guess prev-guess]
   (if (better-good-enough? x current-guess prev-guess)
     current-guess
     (better-sqrt x (improve x current-guess) current-guess))))

(defn count-change
  [amt [first & rest]]
  (cond
    (= amt 0) 1
    (or (not first) (< amt 0)) 0
    :else (+ (count-change amt rest) (count-change (- amt first) (cons first rest)))))

;(count-change 100 [50 25 10 5 1])

(defn f
  [n]
  (if (< n 3)
    n
    (+ (f (dec n))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(defn iter-f
  [n]
  (-> (iterate (fn [[x y z]]
                 [(+ x (* 2 y) (* 3 z)) x y])
               [2 1 0])
      (nth n)
      last))

(defn recursive-pascal
  [rows]
  (if (zero? rows)
    [[1]]
    (let [previous-rows (recursive-pascal (dec rows))
          last-row (last previous-rows)]
      (conj previous-rows (vec (map + (cons 0 last-row) (conj last-row 0)))))))

(defn iterative-pascal
  [rows]
  (->> (iterate (fn [row] (vec (map +' (cons 0 row) (conj row 0)))) [1])
       (take rows)
       (into [])))

(defn expt-iter
  [x n]
  (loop [acc 1
         count n]
    (if (zero? count)
      acc
      (recur (* acc x) (dec count)))))

(defn square [x] (*' x x))
(defn fast-expt
  [x n]
  (cond
    (zero? n) 1
    (even? n) (square (fast-expt x (/ n 2)))
    :else (*' x (fast-expt x (dec n)))))

(defn fast-expt-iter
  [x n]
  (loop [count n
         squares x
         acc 1]
    (cond
      (zero? count) acc
      (even? count) (recur (int (/ count 2)) (square squares) acc)
      :else (recur (dec count) squares (*' acc squares)))))


(defn double-val [a] (+ a a))
(defn half [a] (Math/floor (/ a 2)))
(defn fast-mult
  [a b]
  (cond
    (zero? b) (do (println "here") 0)
    (even? (int b)) (fast-mult (double-val a) (half b))
    :else (+ a (fast-mult a (dec b)))))

(defn fast-mult-iter
  [a b]
  (loop [acc 0
         a a
         b b]
    (cond
      (zero? b) acc
      (even? (int b)) (recur acc (double-val a) (half b))
      :else (recur (+ acc a) (double-val a) (half (dec b))))))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))


(defn fast-prime
  [n]
  (let [rand-divisor (rand-int n)]
    (= rand-divisor (mod (fast-expt-iter rand-divisor n) n))))

(defn little-fermat
  [n times]
  (if (zero? times)
    true
    (and (fast-prime n) (little-fermat n (dec times)))))