import Data.List

diff :: [Int] -> Int
diff (x:y:[]) = abs (x - y)

genparse :: String -> [[Int]]
genparse = map (sort . (map read)) . transpose . map (words) . lines
parse1 = transpose . genparse
parse2 = genparse

part1 = sum . map diff

freq x ys = x*(length $ filter (== x) ys)

part2 (x:y:[]) = sum $ map ((flip freq) y) x

solve :: String -> String
solve i = "Part1 : " ++ p1 ++ "\nPart2 : " ++ p2
   where p1 = (show . part1 $ parse1 i)
         p2 = (show . part2 $ parse2 i)

main = interact $ solve
