import System.Random

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

