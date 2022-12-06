(ns aoc22.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def name-mapping
  (hash-map "A" "rock" "B" "paper" "C" "scissors"
            "X" "rock" "Y" "paper" "Z" "scissors"))

; The winner of the whole tournament is the player with the highest score. Your total score is the sum of your scores for each round. The score for a single round is the score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).
(def game-scoring
  (hash-map "A X" 4
            "A Y" 8
            "A Z" 3
            "B X" 1
            "B Y" 5
            "B Z" 9
            "C X" 7
            "C Y" 2
            "C Z" 6
            "Z Z" 0))

;; In the first round, your opponent will choose Rock (A), and you need the round to end in a draw (Y), so you also choose Rock. This gives you a score of 1 + 3 = 4.
;; In the second round, your opponent will choose Paper (B), and you choose Rock so you lose (X) with a score of 1 + 0 = 1.
;; In the third round, you will defeat your opponent's Scissors with Rock for a score of 1 + 6 = 7.
(def new-game-scoring
  (hash-map "A X" 3 ; need to lose, scissors
            "A Y" 4 ; need to draw, rock
            "A Z" 8 ; need to win, paper
            "B X" 1 ; need to lose, rock
            "B Y" 5 ; need to draw, paper
            "B Z" 9 ; need to win, scissors
            "C X" 2 ; need to lose, paper
            "C Y" 6 ; need to draw, scissors
            "C Z" 7 ; need to win, rock
            ))
(def input (->> (slurp (io/resource "aoc22/day02.txt"))
                (str/split-lines)
                (map str/trim)))

(defn part-1
  "apply the above scoring rules and compute the total score"
  [_]
  (reduce +
          (map game-scoring input)))

(defn part-2
  "compute with the revised scoring"
  [_]
  (reduce +
          (map new-game-scoring input)))
