import Data.List

diagonals []       = []
diagonals ([]:xss) = xss
diagonals xss      = zipWith (++) (map ((:[]) . head) xss ++ repeat [])
                                  ([]:(diagonals (map tail xss)))

allDiagonals x = (diagonals x) ++ (diagonals (reverse x))
allLines i = (lines i) ++ (transpose $ lines i)

allVars i = (allLines i) ++ ((allDiagonals (lines i)))
subVars i = filter (\a -> (3 < length a)) (allVars i)

occs t s = sum [ 1 | r <- tails s, isPrefixOf t r ]

containsXmas i = (occs "XMAS" i) + (occs (reverse "XMAS") i)

checkWord "MAS" = True
checkWord "SAM" = True
checkWord _ = False

filterSmall = filter (\a -> (length a > 2))

hasThing (x:y:xs) = checkWord x && checkWord y
hasThing _ = False

chunks1 :: [String] -> [[String]]
chunks1 (a:b:c:xs) | length a > 2 = f:(chunks1 d)
                   | otherwise = []
   where f = [take 3 a]++[take 3 b]++[take 3 c]
         d = [drop 1 a]++[drop 1 b]++[drop 1 c]
chunks1 _ = []

chunks :: [String] -> [[String]]
chunks [] = []
chunks (x:xs) = (chunks1 (x:xs)) ++ (chunks xs)

part2 = length . filter hasThing . map filterSmall . map allDiagonals . chunks . lines
part1 = sum . map containsXmas . subVars

solve :: String -> String
solve i = "Part1 : " ++ p1 ++ "\nPart2 : " ++ p2
   where p1 = (show $ part1 i)
         p2 = (show $ part2 i)

main = interact $ solve
