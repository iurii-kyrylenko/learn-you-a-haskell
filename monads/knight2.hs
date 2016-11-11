import Control.Monad

data KnightPos = Empty | KnightPos (Int, Int) KnightPos deriving (Show)

isPosValid :: KnightPos -> Bool
isPosValid Empty = False
isPosValid (KnightPos (x, y) _)
  | x < 1 = False
  | y < 1 = False
  | x > 8 = False
  | y > 8 = False
  | otherwise = True

moveKnight :: KnightPos -> [KnightPos]
moveKnight Empty = []
moveKnight h1@(KnightPos(x, y) h) = do
  p <- [ KnightPos (x - 1, y + 2) h1
       , KnightPos (x + 1, y + 2) h1
       , KnightPos (x + 2, y + 1) h1
       , KnightPos (x + 2, y - 1) h1
       , KnightPos (x - 1, y - 2) h1
       , KnightPos (x + 1, y - 2) h1
       , KnightPos (x - 2, y + 1) h1
       , KnightPos (x - 2, y - 1) h1
       ]
  guard (isPosValid p)
  return p

moveKnight3Times :: KnightPos -> [KnightPos]
moveKnight3Times p = return p >>= moveKnight >>= moveKnight >>= moveKnight

path :: (Int, Int) -> (Int, Int) -> [KnightPos]
path s t =
  let ps = moveKnight3Times (KnightPos s Empty)
      pr = \(KnightPos p h) -> (p == t)
  in  filter pr ps
