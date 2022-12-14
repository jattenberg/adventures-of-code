(ns aoc22.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [utils :as utils]
            [clojure.math.combinatorics :as combo]))

(defn coords
  [nrows ncols]
  (combo/cartesian-product (range nrows) (range ncols)))

(defn forest-data
  [trees coords]
  (let [rown (first coords)
        coln (second coords)
        row (nth trees rown)
        col (nth (utils/transpose trees) coln)
        srow (split-at coln row)
        scol (split-at rown col)
        height (nth row coln)
        left (reverse (first srow))
        right (rest (second srow))
        up (reverse (first scol))
        down (rest (second scol))]
    {:height height
     :left left
     :right right
     :up up
     :down down
     :coords coords}))

; To measure the viewing distance from a given tree, look up, down, left, and right from that tree; stop if you reach an edge or at the first tree that is the same height or taller than the tree under consideration. (If a tree is right on the edge, at least one of its viewing distances will be zero.)
(defn direction-visibility
  [height view]
  (utils/take-until #(>= % height) view))

(defn visibility-score
  [frst-data]
  (let [dir-vis (map #(direction-visibility
                       (:height frst-data)
                       (% frst-data))
                     [:left :right :up :down])
        trees-visible (map count dir-vis)]
    (reduce * trees-visible)))

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


(defn parse-visible
  [matrix]
  (map (fn [r] (map second r)) matrix))

(defn is-vis
  [zipped]
  (map (fn [r]
         (map #(or (first %) (second %)) r)) zipped))

(defn visible-trees
  ([matricies] (visible-trees (first matricies) (rest matricies)))
  ([acc matricies]
   (let [side (first matricies)
         zipped (map utils/zip acc side)
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
        rots (utils/rotations trees)
        viss (map visible-on-side rots)
        unrot (utils/invert-rotations viss)
        bool-vis (map parse-visible unrot)
        visible-map (visible-trees bool-vis)]
    (count (filter identity (flatten visible-map)))))

(defn part-2
  "Run with bb -x aoc22.day08/part-2"
  [_]
  (let [trees (->> (io/resource "aoc22/day08.txt")
                   slurp
                   str/split-lines
                   (map parse-row)
                   vec)
        nrows (count trees)
        ncols (count (first trees))
        cds (coords nrows ncols)
        tree-views (map #(forest-data trees %) cds)
        scores (map visibility-score tree-views)]
    (reduce max scores)))
