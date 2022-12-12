(ns aoc22.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [utils :as utils]
            [clojure.core.match :as match]))

(def num-knots (- 10 1))
(def knot-symbols (mapv #(symbol (str (inc %))) (range num-knots)))
(def other-symbols [:H :V])

(defn find-position-of-symbol
  [sym pos]
  (first (utils/find-match-in-2d-array pos #(utils/in? % sym))))

(defn add-symbols
  [pos symbols loc]
  (let [old (get-in pos loc)
        new (vec (set (flatten (conj old symbols))))]
    (assoc-in pos loc new)))

(defn remove-symbols
  [pos symbols loc]
  (let [old (get-in pos loc)
        new (vec (filter #(not (utils/in? symbols %)) old))]
    (assoc-in pos loc new)))

(defn add-col
  "adds a column to the matrix"
  [pos]
  (vec (map #(conj % []) pos)))

(defn move-sym
  [pos sym old-loc new-loc]
  (let [clean-contents (remove-symbols pos [sym] old-loc)
        new-h-pos (add-symbols clean-contents [sym] new-loc)]
    new-h-pos))

(defn maybe-expand-map
  [pos new-loc]
  (let [shape (utils/mat-shape pos)
        new-col (second new-loc)]
    (if (>= new-col (second shape))
      (add-col pos)
      pos)))

(defn maybe-update-knots
  "assume knot denoted by sym-a has moved, does sym-b need to follow?"
  [pos sym-pairs]
  (let [[sym-a sym-b] (first sym-pairs)
        a-loc (find-position-of-symbol sym-a pos)
        b-loc (find-position-of-symbol sym-b pos)
        ttt (println (str a-loc " "  b-loc " " pos))
        dist (abs (- (second a-loc) (second b-loc)))
        new-b-loc [(first a-loc) (inc (second b-loc))]
        _ (println (str sym-a " " sym-b " " "dist: " dist " " (> dist 2) " " (count sym-pairs) " " (utils/mat-shape pos)))
        new-pos (if (> dist 2) (move-sym pos sym-b b-loc new-b-loc) pos)
        visit-update (if (and (> dist 2) (= (count sym-pairs) 1))
                       (add-symbols new-pos [:V] new-b-loc) new-pos)]
    (if (= (count sym-pairs) 1)
      visit-update
      (recur visit-update (rest sym-pairs)))))

(defn update-knots
  [pos]
  (let [syms (concat [:H] knot-symbols)
        sym-pairs (partition 2 1 syms)]
    (maybe-update-knots pos sym-pairs)))

(defn step-right
  "moves the :H one step to the right, expanding the map if necessary
  then updates by dragging :T so that it's next to :H, recording :V
  for any position visited"
  [pos]
  (let [h-pos (find-position-of-symbol :H pos)
        t-pos (find-position-of-symbol :T pos)
        new-loc [(first h-pos) (inc (second h-pos))]
        new-pos (maybe-expand-map pos new-loc)
        moved-head (move-sym new-pos :H h-pos new-loc)]
    (update-knots moved-head)))

(defn _step-down
  [pos]
  (->> (utils/transpose pos)
       (step-right)
       (utils/transpose)))

(defn _step-left
  [pos]
  (->> (utils/reverse-cols pos)
       (step-right)
       (utils/reverse-cols)))

(defn _step-up
  [pos]
  (->> (utils/transpose pos)
       (utils/reverse-cols)
       (step-right)
       (utils/reverse-cols)
       (utils/transpose)))

(defn apply-inst-to-map
  [pos dir x]
  (if (= x 0)
    pos
    (let [updated (match/match [dir]
                    ["R"] (step-right pos)
                    ["D"] (_step-down pos)
                    ["L"] (_step-left pos)
                    ["U"] (_step-up pos))]
      (recur updated dir (dec x)))))

(defn apply-instructions
  [pos instruction]
  (let [dir (first instruction)
        x (second instruction)
        new (apply-inst-to-map pos dir x)]
    new))

(defn parse-instruction
  [ins]
  (let [parts (str/split ins #"\s+")
        direction (first parts)
        steps (parse-long (second parts))]
    [direction steps]))

(defn visited
  [pos]
  (utils/find-match-in-2d-array pos #(utils/in? % :V)))

(defn part-1
  "Run with bb -x aoc22.day09/part-1"
  [_]
  (let [start [[[:H :T :V]]]
        instructions (->> (io/resource "aoc22/day09.txt")
                          (slurp)
                          (str/split-lines)
                          (map parse-instruction))
        steps (reduce apply-instructions start instructions)]
    (println (str "shape: " (utils/mat-shape steps)))
    (count (visited steps))))

(defn part-2
  "Run with bb -x aoc22.day02/part-2"
  [_]
  (let [start [[(concat other-symbols knot-symbols)]]
        instructions (->> (io/resource "aoc22/day09example.txt")
                          (slurp)
                          (str/split-lines)
                          (map parse-instruction))
        steps (reduce apply-instructions start instructions)]
    (println (str/join "\n" steps))
    (println (str "shape: " (utils/mat-shape steps)))
    (count (visited steps))))
