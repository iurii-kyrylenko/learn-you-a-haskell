-------------------------------------------
--    SOLUTION WITH USING CUSTOM TYPES   --
-------------------------------------------
import Data.List

-- data Section = Section
--   { getA :: Int
--   , getB :: Int
--   , getC :: Int
--   } deriving (Show)
data Section = Section Int Int Int deriving (Show)

type RoadSystem = [Section]

data Label = A | B | C deriving (Show)

-- x :: Path
-- x = [(B,10), (C,30), (A,5), (C,20), (B,2), (B,8)]
type Path = [(Label, Int)]

heathrowToLondon :: RoadSystem
heathrowToLondon =
  [ Section 50 10 30
  , Section 5 90 20
  , Section 40 2 25
  , Section 10 8 0
  ]

-- roadStep ([],[]) (Section 50 10 30)
-- ([(C,30),(B,10)],[(B,10)])
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA = sum $ map snd pathA
      priceB = sum $ map snd pathB
      forwardPriceToA = priceA + a
      crossPriceToA = priceB + b + c
      forwardPriceToB = priceB + b
      crossPriceToB = priceA + a + c
      newPathToA = if forwardPriceToA <= crossPriceToA
                    then (A,a):pathA
                    else (C,c):(B,b):pathB
      newPathToB = if forwardPriceToB <= crossPriceToB
                    then (B,b):pathB
                    else (C,c):(A,a):pathA
  in  (newPathToA, newPathToB)

-- optimalPath heathrowToLondon 
-- [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]
optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
  in  if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

-- cat heathrow-to-london.txt | ./min-path2
-- The best path to take is: BCACBBC
-- The price is: 75
main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a,b,c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concat $ map (show . fst) path
      pathPrice = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show pathPrice
