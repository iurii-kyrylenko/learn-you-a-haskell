import Control.Monad
import Text.Read

-- solveRPN :: (Fractional a, Read a) => String -> a
-- solveRPN expression =
--   head $ foldl processItem [] $ words expression

-- processItem :: (Fractional a, Read a) => [a] -> String -> [a]
-- processItem (a:b:xs) "+" = (a + b):xs
-- processItem (a:b:xs) "*" = (a * b):xs
-- processItem (a:b:xs) "-" = (b - a):xs
-- processItem (a:b:xs) "/" = (b / a):xs
-- processItem (a:xs) "n" = (-a):xs
-- processItem stack s =
--   let item = read s :: (Fractional a, Read a) => a in item:stack


solveRPN :: String -> Double
solveRPN = head . foldl processItem [] . words
  where
  processItem (a:b:xs) "+" = (a + b) : xs
  processItem (a:b:xs) "*" = (a * b) : xs
  processItem (a:b:xs) "-" = (b - a) : xs
  processItem (a:b:xs) "/" = (b / a) : xs
  processItem (a:xs) "n" = (-a):xs
  processItem stack s = read s : stack

t1 = solveRPN "42 1 + 53 - n 77 * 10 /"
t2 = solveRPN "42 1 + 53 - n 77 * 10 SORRY"

safeSolveRPN :: String -> Maybe Double
safeSolveRPN = fmap head . foldM safeProcessItem [] . words
-- safeSolveRPN x = (foldM safeProcessItem [] $ words x) >>= return . head
-- safeSolveRPN x = do { xs <- foldM safeProcessItem [] $ words x; return $ head xs }
  where
-- safeProcessItem :: [Double] -> String -> Maybe [Double]
  safeProcessItem (a:b:xs) "+" = return $ (a + b) : xs
  safeProcessItem (a:b:xs) "*" = return $ (a * b) : xs
  safeProcessItem (a:b:xs) "-" = return $ (b - a) : xs
  safeProcessItem (a:b:xs) "/" = return $ (b / a) : xs
  safeProcessItem (a:xs)   "n" = return $ (-a) : xs
  -- safeProcessItem stack s = (readMaybe s) >>= Just . (: stack)
  -- safeProcessItem stack s = fmap (: stack) $ readMaybe s
  -- safeProcessItem stack s = liftM (: stack) $ readMaybe s
  safeProcessItem stack s = (readMaybe s) >>= return . (: stack)

t3 = safeSolveRPN "42 1 + 53 - n 77 * 10 /"
t4 = safeSolveRPN "42 1 + 53 - n 77 * 10 SORRY"