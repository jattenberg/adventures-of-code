(ns aoc22.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :as match]
            [clojure.core.match.regex :as rem]
            [clojure.data.json :as json]))


(defn add-to-path
  "i'd like to do this within the match"
  [path cmd]
  (let [m (re-find #"\$\s+cd\s+(\S+)" cmd)
        f (second m)]
    (conj path f)))

(defn add-dir
  [tree path cmd]
  (let [m (re-find #"dir\s+(\S+)" cmd)
        d (second m)]
    (assoc-in tree (conj path d) {})))

(defn update-sizes
 "just update sizes as we add files"
 [tree s path]
 (let [ts (get-in tree (conj path :siiiize) 0)
       ns (+ s ts)
       nt (assoc-in tree (conj path :siiiize) ns)]
   (if (= 1 (count path))
     nt
     (update-sizes nt s (pop path)))))

 ;(add-file tree path cmd)
(defn add-file
  [tree path cmd]
  (let [m (re-find #"(\d+)\s+(\S+)" cmd)
        s (parse-long (second m))
        f (nth m 2)
        ;nt (assoc-in tree (conj path f) s)
        nt tree]
    (update-sizes nt s path)))

; would be much nicer if i could capture in regex
;(deftest basic-regex
;  (is (= (match ["asdf"]
;           [#"asdf"] 1
;           :else 2)
;        1)))
(defn parse-tree
  "recursively builds the file tree using the shell commands of input"
  ([cmds] (parse-tree cmds {} []))
  ([cmds tree path]
   (if (empty? cmds)
     tree
     (let [cmd (first cmds)
           rest (rest cmds)]
       (match/match [cmd]
         [#"\$\s+cd\s+\.\."] (parse-tree
                              rest
                              tree
                              (pop path))
         [#"\$\s+cd\s+(\S+)"] (parse-tree
                                rest
                                tree
                                (add-to-path path cmd))
         [#"dir\s+(\S+)"] (parse-tree
                            rest
                            (add-dir tree path cmd)
                            path)
         [#"(\d+)\s+\S+"] (parse-tree
                            rest
                            (add-file tree path cmd)
                            path)
         :else (parse-tree rest tree path))))))

(defn part-1
  "Run with bb -x aoc22.day07/part-1"
  [_]
  (let [input (->> (io/resource "aoc22/day07.txt")
                   slurp
                   (str/trim)
                   (str/split-lines)
                   (map str/trim)
                   (remove empty?))
        tree (parse-tree input)]
    ;(println (json/write-str tree))
    tree))

(defn part-2
  "Run with bb -x aoc22.day02/part-2"
  [_]
  (->> input
       (partition-by nil?)
       (take-nth 2)
       (map #(apply + %))
       (sort-by -)
       (take 3)
       (apply +)
       prn))
