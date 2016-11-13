--
-- OLD VERSION
--
-- see also: http://stackoverflow.com/questions/32929252/can-ghc-derive-functor-and-applicative-instances-for-a-monad-transformer
--
-- newtype MyWriter w a = MyWriter { runWriter :: (a, w) }

-- instance (Monoid w) => Monad (MyWriter w) where
--   return x = MyWriter (x, mempty)
--   (MyWriter (x, v)) >>= f =
--     let (MyWriter (y, v1)) = f x
--     in  MyWriter (y, v `mappend` v1)

--
-- NEW VERSION
--
-- http://stackoverflow.com/questions/11684321/how-to-play-with-control-monad-writer-in-haskell
--

import Control.Monad.Writer

t1 = runWriter (return 3 :: Writer String Int)

logNumber :: (Show a) => a -> Writer [String] a
logNumber x = writer (x, ["Got number: " ++ (show x)])

t2 = runWriter $ do
  n1 <- logNumber 42
  n2 <- logNumber 11
  tell ["Gonna multiply these two"]
  return $ n1 + n2

-- gcd' :: Integral t => t -> t -> t
-- gcd' x 0 = abs x
-- gcd' x y =
--   let x' = abs x
--       y' = abs y
--   in  gcd' y' (x' `mod` y')

gcd' :: (Integral t, Show t) => t -> t -> Writer [String] t
gcd' x 0 = do
  tell ["finished with: " ++ (show x)]
  return $ abs x
gcd' x y = do
  tell ["continied with: " ++ (show x) ++ " and " ++ (show y)]
  let x' = abs x
      y' = abs y
  gcd' y' (x' `mod` y')

t3 = runWriter $ gcd' 26424532446 1756728

t4 = mapM_ putStrLn $ snd $ runWriter $ gcd' 26424532446 1756728

gcdReverse :: (Integral t, Show t) => t -> t -> Writer [String] t
gcdReverse x 0 = do
  tell ["finished with: " ++ (show x)]
  return $ abs x
gcdReverse x y = do
  let x' = abs x
      y' = abs y
  result <- gcdReverse y' (x' `mod` y')
  tell ["continied with: " ++ (show x) ++ " and " ++ (show y)]
  return result

t5 = mapM_ putStrLn $ snd $ runWriter $ gcdReverse 26424532446 1756728
