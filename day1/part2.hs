import Data.List (isInfixOf)
import Data.Char (isDigit)

-- Part 2.
calibrate :: String -> String -> [Integer]
calibrate [] acc = filter (/=0) [max (tryLetter acc) (tryLetter acc)]
calibrate (x:xs) acc
  | num /= 0  = num : calibrate xs [x]
  | otherwise = calibrate xs newAcc
  where 
    newAcc = acc ++ [x]
    num
      | isDigit x = read [x]
      | length newAcc < 3 = 0 -- Minimum length of a number letter is 3
      | otherwise = tryLetter newAcc

tryLetter :: String -> Integer
tryLetter word 
  | "one"   `isInfixOf` word  = 1
  | "two"   `isInfixOf` word = 2
  | "three" `isInfixOf` word = 3
  | "four"  `isInfixOf` word = 4
  | "five"  `isInfixOf` word = 5
  | "six"   `isInfixOf` word = 6
  | "seven" `isInfixOf` word = 7
  | "eight" `isInfixOf` word = 8
  | "nine"  `isInfixOf` word = 9
  | otherwise = 0

-- Concatenates two numeric values
addDigit :: (Ord a, Num a) => a -> a -> a
addDigit a b | a < 0     = a * 10 - b
             | otherwise = a * 10 + b

-- Recover the first and last numeric values
-- and concatenate them together
recover :: [Integer] -> Integer
recover xs = addDigit (head xs) (last xs)

main :: IO ()
main = do
  contents <- readFile "puzzle.txt"
  let fileLines = lines contents
  let result = sum $ map (recover . (`calibrate` "")) fileLines
  print result
