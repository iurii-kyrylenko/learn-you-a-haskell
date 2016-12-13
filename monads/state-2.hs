import Control.Applicative
import System.Random
import Control.Monad.Trans.State

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftA2 (,) try try where try = randomRIO (1, 6)

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n =
  let t n | n < 1     = []
          | otherwise = (randomRIO (1, 6)) : t (n - 1)
  in sequenceA (t n)

rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice g = ((x, y), g2)
  where (x, g1) = randomR (1, 6) g
        (y, g2) = randomR (1, 6) g1

t1 = rollDice $ mkStdGen 0

rs :: State StdGen Int
rs = state $ randomR (1, 6)

rd = runState $ do
  x <- rs
  y <- rs
  return (x, y)

rd' = runState $ rs >>= \x -> rs >>= \y -> return (x, y)

t2 = rd $ mkStdGen 0
t2' = rd' $ mkStdGen 0

rd'' = runState $ liftA2 (,) rs rs
t2'' = rd'' $ mkStdGen 0

rollNDice :: Int -> State StdGen [Int]
rollNDice n =
  let t n | n < 1     = []
          | otherwise = rs : t (n-1)
  in sequenceA (t n)

diceSequense :: Int -> Int -> [Int] 
diceSequense n seed = evalState (rollNDice n) (mkStdGen seed)

t3 = diceSequense 200 42
t4 = diceSequense 200 43
t5 = diceSequense 200 44
