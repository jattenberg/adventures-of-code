module Main where

main = readFile "resources/day01.txt" >>= \input -> let (_, a, b) = solve $ filter (not . null) $ lines input in putStrLn $ "1a: " ++ show a ++ "\n1b: " ++ show b

solve = foldl step (50, 0, 0)
  where step (pos, cnt1, cnt2) s = let (dir:n) = s; d = read n; newPos = mod (if dir == 'L' then pos - d else pos + d) 100; zeros = countZeros pos d (dir == 'L') in (newPos, cnt1 + if newPos == 0 then 1 else 0, cnt2 + zeros)
        countZeros p d isLeft = length $ filter (== 0) $ map (`mod` 100) $ if isLeft then [p-1, p-2..p-d] else [p+1..p+d]
