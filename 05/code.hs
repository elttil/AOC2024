import Data.List
import Data.Char

takeUntil x xs = takeWhile (\a -> (x /= a)) xs
dropUntil x xs = dropWhile (\a -> (x /= a)) xs

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

precedent :: Eq a => [a] -> [a] -> Bool
precedent (a:b:[]) pages | not (elem a pages) = True
                         | not (elem b rest) = True
                         | 0 == length rest = True
                         | otherwise = False
   where
   rest = takeUntil a pages

swapByValue :: Eq a => a -> a -> [a] -> [a]
swapByValue _ _ [] = []
swapByValue x y xs = map swap xs
  where
    swap z
      | z == x = y
      | z == y = x
      | otherwise = z

middle :: [a] -> [a]
middle l@(_:_:_:_) = middle $ tail $ init l
middle l           = l

follows :: Eq a => [[a]] -> [a] -> Bool
follows [] _ = True
follows (x:xs) pages = precedent x pages && follows xs pages

part1 :: String -> Int
part1 i = sum . map (read . concat . middle) $ filter (follows rules) pages
   where
   input = lines i
   rules = map (wordsWhen (=='|')) $ takeUntil "" input
   pages = map (wordsWhen (==',')) $ tail $ dropUntil "" input

fixer :: Eq a => [[a]] -> [a] -> [a]
fixer [] pages = pages
fixer (x:xs) pages | not $ precedent [a,b] pages = fixer xs (swapByValue a b pages)
                   | otherwise = fixer xs pages
  where [a,b] = x

keepRunning rules b | follows rules result = result
                    | otherwise = keepRunning rules result
   where result = fixer rules b

part2 :: String -> Int
part2 i = sum . map (read . concat . middle) . map (keepRunning rules) $ filter (not . follows rules) pages
   where
   input = lines i
   rules = map (wordsWhen (=='|')) $ takeUntil "" input
   pages = map (wordsWhen (==',')) $ tail $ dropUntil "" input

solve :: String -> String
solve i = "Part1 : " ++ p1 ++ "\nPart2 : " ++ p2
   where p1 = (show $ part1 i)
         p2 = (show $ part2 i)

main = interact $ solve
