import Control.Monad

type KnightPos = (Int, Int)

isPosValid :: KnightPos -> Bool
isPosValid (x, y)
  | x < 1 = False
  | y < 1 = False
  | x > 8 = False
  | y > 8 = False
  | otherwise = True

moveKnight :: KnightPos -> [KnightPos]
moveKnight (x, y) = do
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
moveKnight3Times p = return p >>= moveKnight >>= moveKnight >>= moveKnight

isTargetAccessibleIn3Moves :: KnightPos -> KnightPos -> Bool
isTargetAccessibleIn3Moves s t = t `elem` (moveKnight3Times s)

moveKnightFn :: Int -> KnightPos -> [KnightPos]
moveKnightFn n = foldl (<=<) return $ replicate n moveKnight
-- moveKnightFn n = foldl (<=<) (return . id) $ replicate n moveKnight

moveKnightEx :: Int -> KnightPos -> [KnightPos]
moveKnightEx p = return p >>= moveKnightFn

isTargetAccessibleEx :: Int -> KnightPos -> KnightPos -> Bool
isTargetAccessibleEx n s t = t `elem` moveKnightEx n s