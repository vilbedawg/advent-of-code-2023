import Data.Char (isDigit)

data Point = Point { x :: Int, y :: Int } deriving (Show, Eq)
data Node = Node { value :: Int, leftEdge :: Point, rightEdge :: Point } deriving (Show, Eq)

isSymbol :: Char -> Bool
isSymbol ch = ch `elem` "@#$%&*-=+/"

findSymbols rows f = concatMap (\(y, row) -> points row 0 y) (zip [0..] rows)
  where 
    points :: String -> Int -> Int -> [Point]
    points [] _ _ = []
    points (column:rest) x y = 
      if f column then
        Point x y : points rest (x+1) y
      else 
        points rest (x+1) y


findNodes :: [String] -> [Node]
findNodes rows = concatMap (\(y, row) -> nodes row 0 y) (zip [0..] rows)
  where 
    nodes :: String -> Int -> Int -> [Node]
    nodes [] _ _ = []
    nodes s@(c:cs) x y = 
      if isDigit c then  
        let 
          (digits, remaining) = span isDigit s
          x' = x + length digits
          newNode = Node { value = read digits, leftEdge = Point (x-1) y, rightEdge = Point x' y }
        in newNode : nodes remaining x' y
      else 
        nodes cs (x+1) y

gearRatio :: Point -> [Node] -> Int
gearRatio p@(Point x y) nodes
  | length adjacentNodes == 2 = product $ map value adjacentNodes
  | otherwise = 0 
  where adjacentNodes = filter (isPartNumber p) nodes 

isPartNumber :: Point -> Node -> Bool
isPartNumber point (Node _ leftEdge@(Point lx ly) rightEdge@(Point rx ry)) = 
  point == leftEdge || 
  point == rightEdge || 
  isBetween point leftTop rightTop || 
  isBetween point leftBottom rightBottom
  where 
    leftTop = Point lx (ly - 1)
    leftBottom = Point lx (ly + 1)
    rightTop = Point rx (ry - 1)
    rightBottom = Point rx (ry + 1)

-- Check if a node has any adhacent symbols
partNumber :: Node -> [Point] -> Int
partNumber node@(Node value _ _) symbols
  | any (`isPartNumber` node) symbols = value
  | otherwise = 0

isBetween :: Point -> Point -> Point -> Bool
isBetween (Point px py) (Point lx ly) (Point rx ry) =
  px >= lx && px <= rx && py >= ly && py <= ry

main :: IO ()
main = do
  contents <- readFile "puzzle.txt"
  let fileLines = lines contents
  let nodes = findNodes fileLines

  -- Part 1
  let symbols = findSymbols fileLines isSymbol
  let partNumbers = sum $ map (`partNumber` symbols) nodes
  print partNumbers

  -- Part 2
  let symbols = findSymbols fileLines (== '*')
  let gearRatios = sum $ map (`gearRatio` nodes) symbols
  print gearRatios

  return ()
