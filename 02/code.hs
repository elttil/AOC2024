import Data.List

parse :: String -> [[Int]]
parse = map (map read) . map words . lines

inc :: [Int] -> Bool
inc (x:y:xs)
   | (y > x) && (y - x) < 4 = inc (y:xs)
   | otherwise = False
inc _ = True

dec :: [Int] -> Bool
dec (x:y:xs)
   | (x > y) && (x - y) < 4 = dec (y:xs)
   | otherwise = False
dec _ = True

isSafe i = (inc i) || (dec i)

fuckit [] = []
fuckit (x:xs) = xs : map (x :) (fuckit xs)

isSafeSec = or . map isSafe . fuckit

part1 :: [[Int]] -> Int
part1 = length . filter (isSafe)

part2 :: [[Int]] -> Int
part2 = length . filter (isSafeSec)

solve :: String -> String
solve i = "Part1 : " ++ p1 ++ "\nPart2 : " ++ p2
   where p1 = (show . part1 $ parse i)
         p2 = (show . part2 $ parse i)

main = interact $ solve
