import Control.Monad.Writer

t1 = filter (<4) [1..10]

loggedCondition :: Bool -> Writer [String] Bool
loggedCondition True  = writer (True, ["I got it!"])
loggedCondition False = writer (False, ["I ain't got it!"])

t2 = runWriter $ loggedCondition True

myPredicate :: (Num a, Ord a) => a -> Writer [String] Bool
myPredicate = loggedCondition . (<4)

t3 = runWriter $ filterM myPredicate [1..10]

t4 = fst t3
t5 = mapM_ putStrLn $ snd t3

yourPredicate :: Int -> Writer [String] Bool
yourPredicate x
  | x < 4 = writer (True, [(show x) ++ ": I got it!"])
  | otherwise = writer (False, [(show x) ++ ": I ain't got it!"])

t6 = mapM_ putStrLn $ snd $ runWriter $ filterM yourPredicate [1..10]

powerset :: [Int] -> [[Int]]
powerset = filterM $ \x -> [True, False]

t7 = mapM_ (putStrLn . show) $ powerset [1..3]