import Data.Char (isSpace, isDigit)

main :: IO ()
main = do
  contents <- readFile "puzzle.txt"
  let fileLines = lines contents
  let games = map parseGame fileLines
  let wins = sum $ map isPossibleGame games
  print wins
  let powerOfCubes = map powerOfCubeSet games
  let power = sum $ map snd powerOfCubes
  print power


type Cube = (String, Integer)
type Game = (Integer, [Cube])

isPossibleGame :: Game -> Integer
isPossibleGame (indx, game) = if all isPossibleCount game then indx else 0

isPossibleCount :: Cube -> Bool
isPossibleCount ("red", n) = n <= 12 
isPossibleCount ("green", n) = n <= 13 
isPossibleCount ("blue", n) = n <= 14

powerOfCubeSet :: Game -> (Integer, Integer)
powerOfCubeSet (indx, cubes) = 
  let maxReds   = maxCount . filter (\(c, v) -> c == "red") $ cubes
      maxGreens = maxCount . filter (\(c, v) -> c == "green") $ cubes
      maxBlues  = maxCount . filter (\(c, v) -> c == "blue")  $ cubes
  in (indx, maxReds * maxGreens * maxBlues)

maxCount :: [Cube] -> Integer
maxCount (x:xs) = 
  foldr (\(_, v) accV -> if v > accV then v else accV) (snd x) xs

-- Parse the game into a list of cubes with the game index.
-- I.e., [1, ("blue",48),("red",40),("green",37)]
parseGame :: String -> Game
parseGame input = 
    let gameInfo = parseGameInfo input
        gameContent = parseGameContent $ snd gameInfo
    in (fst gameInfo, gameContent)

-- Parse the game index and the contents of the game.
parseGameInfo :: String -> (Integer, String)
parseGameInfo game = 
  let (indexStr, rest) = break (== ':') game
      index = read $ filter isDigit indexStr
  in (index, filter (not . isSpace) $ drop 2 rest)

-- Split a set of cubes string into list of cubes with the total sum for each color
parseGameContent :: String -> [Cube]
parseGameContent = parseCubes . map (wordsWhen (==',')) . wordsWhen (==';')

-- Returns a tuple form of the cube. I.e.,
-- "1red" -> ("red", 1)
parseCubes :: [[String]] -> [Cube]
parseCubes = concatMap (map toCube)

toCube:: String -> Cube
toCube str =
  case span isDigit str of
    (amount, "blue") -> ("blue", read amount)
    (amount, "red") -> ("red", read amount)
    (amount, "green") -> ("green", read amount)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = 
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

