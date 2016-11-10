import Control.Monad

a = (*) <$> [1,2,3] <*> [10, 100, 1000]

t1 = [1,2,3] >>= \x -> [10, 100, 1000] >>= \y -> return (x * y)

t2 = [1,2,3] >>= (\x -> [10, 100, 1000] >>= (\y -> return (x * y)))

t3 = [1,2,3] >>= (\x -> [10, 100, 1000] >>= return . (x*))

t4 = do
  x <- [1,2,3]
  y <- [10, 100, 1000]
  return (x * y)

t5 = [x * y | x <- [1,2,3], y <- [10,100,1000]]

-------------------------

f1 = [x | x <- [1..50], '7' `elem` show x]

-- in the list context: guard b is [()] if b is True, and [] if b is False.

f2 = [1..50] >>= (\x -> guard ('7' `elem` show x) >>= (\y -> return x))

f3 = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)

f4 = do
  x <- [1..50]
  guard ('7' `elem` show x)
  return x -- uses previous result (each x doesn't combine with empty elements)

a1 = [1..10] >>= \x -> [] >> return x
a2 = [1..10] >>= \x -> [()] >> return x
a3 = [1..10] >>= \x -> ["dummy"] >> return x
a4 = [1..10] >>= \x -> (if (odd x) then ["dummy"] else []) >> return x
a5 = [1..10] >>= \x -> (if (odd x) then ["dummy"] else []) >>= \y -> return x
a6 = [1..10] >>= \x -> guard (odd x) >> return x
a7 = do { x <- [1..10]; guard $ odd x; return x }
a8 = [x | x <- [1..10], odd x]

-------------------------

type KnightPos = (Int, Int)

isPosValid :: KnightPos -> Bool
isPosValid (x, y)
  | x < 1 = False
  | y < 1 = False
  | x > 8 = False
  | y > 8 = False
  | otherwise = True

isPosValid' :: KnightPos -> Bool
isPosValid' (x, y) = x `elem` [1..8] && y `elem` [1..8]

moveKnight :: KnightPos -> [KnightPos]
moveKnight (x, y) =
  [ (x - 1, y + 2)
  , (x + 1, y + 2)
  , (x + 2, y + 1)
  , (x + 2, y - 1)
  , (x - 1, y - 2)
  , (x + 1, y - 2)
  , (x - 2, y + 1)
  , (x - 2, y - 1)
  ] >>= \p -> guard (isPosValid p) >> return p

moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (x, y) =
  let all = [ (x - 1, y + 2)
            , (x + 1, y + 2)
            , (x + 2, y + 1)
            , (x + 2, y - 1)
            , (x - 1, y - 2)
            , (x + 1, y - 2)
            , (x - 2, y + 1)
            , (x - 2, y - 1)
            ]
  in [p | p <- all, isPosValid p]

moveKnight'' :: KnightPos -> [KnightPos]
moveKnight'' (x, y) = do
  p <- [ (x - 1, y + 2)
       , (x + 1, y + 2)
       , (x + 2, y + 1)
       , (x + 2, y - 1)
       , (x - 1, y - 2)
       , (x + 1, y - 2)
       , (x - 2, y + 1)
       , (x - 2, y - 1)
       ]
  guard (isPosValid p)
  return p

moveKnight3Times :: KnightPos -> [KnightPos]
moveKnight3Times p = do
  -- p0 <- return p
  p1 <- moveKnight p
  p2 <- moveKnight p1
  moveKnight p2

moveKnight3Times' :: KnightPos -> [KnightPos]
moveKnight3Times' p = moveKnight p >>= moveKnight >>= moveKnight

moveKnight3Times'' :: KnightPos -> [KnightPos]
moveKnight3Times'' p = return p >>= moveKnight >>= moveKnight >>= moveKnight

isTargetAccessibleIn3Moves :: KnightPos -> KnightPos -> Bool
isTargetAccessibleIn3Moves s t = t `elem` (moveKnight3Times s)

