module Main where

main = readFile "resources/day01.txt" >>= print . solve . filter (not . null) . lines

solve = snd . foldl step (50, 0)
  where step (pos, cnt) s = let (dir:n) = s; d = read n; p = mod (if dir == 'L' then pos - d else pos + d) 100 in (p, cnt + if p == 0 then 1 else 0)
