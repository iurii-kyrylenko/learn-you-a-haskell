import System.Random
import Control.Monad.State

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, gen1) = random gen
      (secondCoin, gen2) = random gen1
      (thirdCoin, _) = random gen2
  in  (firstCoin, secondCoin, thirdCoin)

s1 = threeCoins $ mkStdGen 1


type Stack = [Int]

push :: Int -> Stack -> ((), Stack)
push x xs = ((), x:xs)

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

----------

getRandom :: State StdGen Bool
getRandom = state random

process = do
  r1 <- getRandom
  r2 <- getRandom
  r3 <- getRandom
  return (r1, r2, r3)

threeCoins' seed = fst $ runState process $ mkStdGen seed

s2 = threeCoins' 1
