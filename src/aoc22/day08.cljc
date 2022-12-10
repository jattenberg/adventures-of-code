(ns aoc22.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as cset]))

(defn transpose
  "takes a seq of seqs and transposes it"
  [x]
  (apply mapv vector x))

(defn visible-in-row
  "which trees are visible at each depth in a row"
  [row]
  (reduce (fn [acc x]
            (let [lst-val (first (last acc))
                  mx (max lst-val x)
                  gt (> x lst-val)]
              (conj acc [mx gt])))
          [[(first row) true]]
          (rest row)))

(defn visible-on-side
  [matrix]
  (map visible-in-row matrix))

(defn count-visible-on-side
  [matrix]
  (reduce + (map #(count (set %)) matrix)))

(defn rotations
  [matrix]
  [matrix
   (transpose matrix)
   (map reverse matrix)
   (map reverse (transpose matrix))])

(defn invert-rotations
  [matricies]
  [(nth matricies 0)
   (transpose (nth matricies 1))
   (map reverse (nth matricies 2))
   (transpose (map reverse (nth matricies 3)))])

(defn parse-visible
  [matrix]
  (map (fn [r] (map second r)) matrix))

(defn zip [& colls]
  (partition (count colls) (apply interleave colls)))

(defn is-vis
  [zipped]
  (map (fn [r]
         (map #(or (first %) (second %)) r)) zipped))

(defn visible-trees
  ([matricies] (visible-trees (first matricies) (rest matricies)))
  ([acc matricies]
   (let [side (first matricies)
         zipped (map zip acc side)
         vis (is-vis zipped)]
    (if (= 1 (count matricies))
      vis
      (visible-trees vis (rest matricies))))))

(defn parse-row
  [row]
  (->> (str/split row #"")
       (map parse-long)
       vec))


(defn part-1
  "Run with bb -x aoc22.day08/part-1"
  [_]
  (let [trees (->> (io/resource "aoc22/day08.txt")
                   slurp
                   str/split-lines
                   (map parse-row)
                   vec)
        rots (rotations trees)
        viss (map visible-on-side rots)
        unrot (invert-rotations viss)
        bool-vis (map parse-visible unrot)
        visible-map (visible-trees bool-vis)]
    (count (filter identity (flatten visible-map)))))


