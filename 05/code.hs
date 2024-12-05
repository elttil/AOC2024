import Data.List

takeUntil x xs = takeWhile (\a -> (x /= a)) xs
dropUntil x xs = dropWhile (\a -> (x /= a)) xs

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

ordering :: Eq a => [[a]] -> a -> a -> Ordering
ordering [] x y = EQ
ordering ((a:b:[]):xs) x y
   | a == x && b == y = LT
   | a == y && b == x = GT
   | otherwise = ordering xs x y

middle :: [a] -> [a]
middle l@(_:_:_:_) = middle $ tail $ init l
middle l           = l

follows rules pages = (sortBy (ordering rules) pages) == pages

part1 :: String -> Int
part1 i = sum . map (read . concat . middle) $ filter (follows rules) pages
   where
   input = lines i
   rules = map (wordsWhen (=='|')) $ takeUntil "" input
   pages = map (wordsWhen (==',')) $ tail $ dropUntil "" input

part2 :: String -> Int
part2 i = sum . map (read . concat . middle) . map (sortBy (ordering rules)) $ filter (not . follows rules) pages
   where
   input = lines i
   rules = map (wordsWhen (=='|')) $ takeUntil "" input
   pages = map (wordsWhen (==',')) $ tail $ dropUntil "" input

solve :: String -> String
solve i = "Part1 : " ++ p1 ++ "\nPart2 : " ++ p2
   where p1 = (show $ part1 i)
         p2 = (show $ part2 i)

main = interact $ solve
