import GHC.Base
import Data.List
import Data.Ratio

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving (Show)

p = Prob [(1, 1 % 3), (2, 2 % 3)]

instance Functor Prob where
  fmap f (Prob xs) = Prob $ fmap (\(x, p) -> (f x, p)) xs

p1 = fmap odd p

p2 = Prob
  [ (Prob [(1, 1 % 3), (2, 2 % 3)], 1 % 5)
  , (Prob [(2, 1 % 4), (3, 3 % 4)], 4 % 5)
  ]

multiplyProb :: (Prob a, Rational) -> [(a, Rational)]
multiplyProb ((Prob xs), p) = fmap (\(x, q) -> (x, p * q)) xs

p3 = multiplyProb (p1, 1 % 2)

joinProb :: Prob (Prob a) -> Prob a
joinProb (Prob xs) = Prob $ concat $ fmap multiplyProb xs

p4 = joinProb p2

---------------------------
joinProb' :: Prob (Prob a) -> [(a, Rational)]
joinProb' (Prob xs) = concat $ fmap multiplyProb xs

p4' = joinProb' p2

groupProb :: Eq a => [(a, Rational)] -> [[(a, Rational)]]
groupProb = groupBy $ \(x, p) (y, q) -> x == y

p5 = groupProb p4'

addProb :: [(a, Rational)] -> (a, Rational)
addProb xs = (fst $ head xs, foldl (\a (x, p) -> a + p) 0 xs)

groupAndAddProb :: Eq a => [(a, Rational)] -> [(a, Rational)]
groupAndAddProb xs = fmap addProb $ groupProb xs

joinProbWithGroup :: Eq a => Prob (Prob a) -> Prob a
joinProbWithGroup = Prob . groupAndAddProb . joinProb'

p6 = joinProbWithGroup p2

normalizeProb :: Eq a => Prob a -> Prob a
normalizeProb (Prob xs) = Prob $ groupAndAddProb xs
---------------------------

instance Monad Prob where
  return x = Prob [(x, 1 % 1)]

  -- Can't add (Eq b) to the context of the type signature for:
  -- (>>=) :: Prob a -> (a -> Prob b) -> Prob b

  -- (>>=) m f = joinProbWithGroup $ fmap f m

  (>>=) m f = joinProb $ fmap f m

instance Applicative Prob where
  pure = return
  (<*>) = ap

data Coin = Heads | Tails deriving (Show, Eq)

coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

p7 = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return $ all (==Tails) [a,b,c]

p8 = normalizeProb p7
