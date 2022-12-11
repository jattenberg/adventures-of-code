(ns aoc22.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [utils :as utils]
            [clojure.core.match :as match]))

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

(defn step-right
  "moves the :H one step to the right, expanding the map if necessary
  then updates by dragging :T so that it's next to :H, recording :V
  for any position visited"
  [pos]
  (let [h-pos (find-position-of-symbol :H pos)
        t-pos (find-position-of-symbol :T pos)
        cols-ct (count (first pos))
        new-loc [(first h-pos) (inc (second h-pos))]
        new-pos (if (>= cols-ct (second new-loc))
                  (add-col pos)
                  pos)
        moved-head (move-sym new-pos :H h-pos new-loc)
        t-dist (- (second new-loc) (second t-pos))]
    (if (< t-dist 2)
      moved-head
      (->> (move-sym
            moved-head
            :T
            t-pos
            [(first h-pos) (inc (second t-pos))])
           (#(add-symbols % [:V] [(first h-pos) (inc (second t-pos))]))))))

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
      (apply-inst-to-map updated dir (dec x)))))

(defn apply-instructions
  [steps instruction]
  (let [dir (first instruction)
        x (second instruction)
        pos (last steps)
        new (apply-inst-to-map pos dir x)]
    (conj steps new)))

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
        steps (reduce apply-instructions [start] instructions)]
    (->> (last steps)
         (visited)
         (count))))

(defn part-2
  "Run with bb -x aoc22.day02/part-2"
  [_]
  (->> (io/resource "aoc22/day09example.txt")
       (slurp)
       (str/split-lines)
       (partition-by nil?)
       (take-nth 2)
       (map #(apply + %))
       (sort-by -)
       (take 3)
       (apply +)
       prn))
