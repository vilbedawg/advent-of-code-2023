module Main where
import Data.Char (isDigit, isSpace)
import Debug.Trace (trace)

data Card = 
  Card 
  { index :: Int
  , winning :: [Int]
  , own :: [Int]
  , copies :: Int 
  } deriving (Show)

parseCard :: String -> Card
parseCard line = Card { index = cardIndex, winning = winning, own = own, copies = 1 }
  where
    cardIndex = read $ filter isDigit $ takeWhile (/= ':') line
    rest = tail $ dropWhile (/= ':') line
    (winningString, ownString) = span (/= '|') rest
    own = parseDigits $ tail ownString
    winning = parseDigits winningString

parseDigits :: String -> [Int]
parseDigits [] = []
parseDigits (x:xs) =
  case takeWhile isDigit (x:xs) of
    "" -> parseDigits xs
    digits -> read digits : parseDigits (dropWhile isDigit (x:xs))
 
 -- Part 1
totalpoints :: [Int] -> Int
totalpoints [] = 0
totalpoints (x:xs) = go x 1 + totalpoints xs 
  where 
    go :: Int -> Int -> Int
    go x n
      | x <= 0 = 0
      | otherwise = max 1 n * max 1 (go (x-1) 2)

winners :: Card -> Int
winners card = go card 0
  where
    go :: Card -> Int -> Int
    go (Card _ _ [] _) n = n
    go (Card i winning (own:rest) copies) n
      | own `elem` winning = go (Card i winning rest copies) (n + 1)
      | otherwise = go (Card i winning rest copies) n


-- Part 2.
mapCards :: [Card] -> [Card]
mapCards [] = []
mapCards cards@(card@(Card i winning own copies):rest)
  | winnerAmount == 0 = card : mapCards rest
  | otherwise = card : mapCards (cardCopies ++ restAfterCopies)
  where
    winnerAmount = winners card
    cardCopies = makeCopies rest winnerAmount copies
    restAfterCopies = drop (length cardCopies) rest


makeCopies :: [Card] -> Int -> Int -> [Card]
makeCopies [] _ _ = []
makeCopies cards@(card@(Card i w o copies):rest) n amount
  | n > 0 = Card i w o (copies + amount) : makeCopies rest (n-1) amount
  | otherwise = cards


main :: IO ()
main = do
  contents <- readFile "puzzle.txt"
  let fileLines = lines contents
  let cards = fmap parseCard fileLines

  -- Part 1.
  let winnerPoints = totalpoints $ map winners cards
  print winnerPoints

  -- Part 2.
  let cards2 = mapCards cards
  print $ sum $ map copies cards2
  return ()
