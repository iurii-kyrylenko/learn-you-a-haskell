solveRPN :: (Fractional a, Read a) => String -> a
solveRPN expression =
  head $ foldl processItem [] $ words expression

processItem :: (Fractional a, Read a) => [a] -> String -> [a]
processItem (a:b:xs) "+" = (a + b):xs
processItem (a:b:xs) "*" = (a * b):xs
processItem (a:b:xs) "-" = (b - a):xs
processItem (a:b:xs) "/" = (b / a):xs
processItem (a:xs) "n" = (-a):xs
processItem stack s =
  let item = read s :: (Fractional a, Read a) => a in item:stack


-- solveRPN :: String -> Double
-- solveRPN = head . foldl processItem [] . words
--   where
--   processItem (a:b:xs) "+" = (a + b) : xs
--   processItem (a:b:xs) "*" = (a * b) : xs
--   processItem (a:b:xs) "-" = (b - a) : xs
--   processItem (a:b:xs) "/" = (b / a) : xs
--   processItem stack s = read s : stack
