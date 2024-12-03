{-# LANGUAGE ViewPatterns #-}
import Data.Char
import Data.List

removal :: String -> Bool -> String
removal (stripPrefix "do()" -> Just xs) False = removal xs True
removal (stripPrefix "don't()" -> Just xs) True = removal xs False
removal (x:xs) False = removal xs False
removal (x:xs) True = x : removal xs True
removal [] _ = []

part1 :: String -> Int
part1 ('m':'u':'l':'(':xs)
   | delim /= ',' = part1 xs
   | (head rest2) /= ')' = part1 xs
   | (0 == length a) || (0 == length b) = part1 xs
   | otherwise = (read a)*(read b)+part1 xs
   where
   a = takeWhile isDigit xs
   rest = dropWhile isDigit xs
   delim = head rest
   b = takeWhile isDigit (tail rest)
   rest2 = dropWhile isDigit (tail rest)
part1 (x:xs) = part1 xs
part1 _ = 0

solve :: String -> String
solve i = "Part1 : " ++ p1 ++ "\nPart2 : " ++ p2
   where p1 = (show . part1 $ i)
         p2 = (show . part1 $ removal i True)

main = interact $ solve
