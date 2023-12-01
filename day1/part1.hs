import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)

-- Part 1.
calibrate :: String -> Integer
calibrate xs = 
  case digits of
    []       -> 0
    [x]      -> read [x, x]
    (x:rest) -> read [x, last rest]
  where digits = filter isDigit xs

processList :: [String] -> Integer
processList = sum . map calibrate

main1 :: IO ()
main1 = do
  contents <- readFile "puzzle.txt"
  let fileLines = lines contents
  let result = processList fileLines
  print result

