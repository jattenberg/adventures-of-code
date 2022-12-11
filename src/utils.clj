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

(defn invert-rotations
  [matricies]
  [(nth matricies 0)
   (transpose (nth matricies 1))
   (map reverse (nth matricies 2))
   (transpose (map reverse (nth matricies 3)))])
