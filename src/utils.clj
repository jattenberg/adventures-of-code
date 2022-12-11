(ns utils
     (:require [clojure.java.io :as io]
               [clojure.string :as str]))

(defn zip [& colls]
  (partition (count colls) (apply interleave colls)))

(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) nil)
       (cons (first s) (take-until pred (rest s)))))))

(defn reverse-cols
  "in a 2d array, reverses the column order"
  [x]
  (->> (map reverse x)
       (map #(into [] %))
       vec))

(defn transpose
  "takes a seq of seqs and transposes it"
  [x]
  (apply mapv vector x))

(defn rotations
  [matrix]
  [matrix
   (transpose matrix)
   (map reverse matrix)
   (map reverse (transpose matrix))])

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn find-match-in-2d-array
  "returns the index of a 2d array where the condition of the fn
  `matcher` is satisfied"
  [input matcher]
  (for [[x row] (map-indexed vector input)
        [y val] (map-indexed vector row)
      :when (matcher val)]
    [x y]))

(defn invert-rotations
  [matricies]
  [(nth matricies 0)
   (transpose (nth matricies 1))
   (map reverse (nth matricies 2))
   (transpose (map reverse (nth matricies 3)))])
