(ns algorithms.dining-philosophers
  (:require [clojure.core.async :refer [chan go go-loop <!! >!!]]))

(def forks (repeatedly 5 #(ref false)))

(def max-sleep-time (* 5 1000))

(defn sleep
  []
  (Thread/sleep (rand-int max-sleep-time)))

(def log-channel (chan))

(defn log [& x]
  (>!! log-channel (apply str x)))
(go-loop []
  (println (<!! log-channel))
  (recur))

(defn left-fork
  [n]
  (nth forks n))

(defn right-fork
  [n]
  (->> (mod (inc n) (count forks))
       (nth forks)))

(defn grab-fork
  [fork]
  (if @fork
    false
    (ref-set fork true)))

(defn release-fork [fork]
  (ref-set fork false))

(defn grab-forks [n]
  (dosync
    (if (grab-fork (left-fork n))
      (if (grab-fork (right-fork n))
        true
        (release-fork (left-fork n)))

      false)))

(defn release-forks
  [n]
  (dosync
    (ref-set (left-fork n) false)
    (ref-set (right-fork n) false)))

(defn think
  [n]
  (log "Philosopher " n " is thinking")
  (sleep))

(defn eat
  [n]
  (log "Philosopher " n " is eating")
  (sleep)
  (log "Philosopher " n " finished eating")
  (release-forks n))

(defn philosopher
  [n]
  (while true
    (if (grab-forks n)
      (eat n))
    (think n)))

(defn begin
  []
  (doseq [i (range 5)]
    (-> (Thread. #(philosopher i))
        .start)))
