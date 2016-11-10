main = do
  putStrLn "----------"
  print (mfib 10000)
  putStrLn "----------"
  print (fib 10000)
  putStrLn "----------"
-----
mfib :: Int -> Integer
mfib = (map fib [0 ..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = mfib (n-2) + mfib (n-1)
-----
fib = (fibs !!)
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-----
