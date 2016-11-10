class MyFunctor cons where
  fmap' :: (a -> b) -> cons a -> cons b

instance MyFunctor IO where
  fmap' f action = do
    result <- action
    return $ f result

r1 = do
  ln <- getLine
  let ln' = reverse ln
  putStrLn ln'

r2 = do
  ln <- fmap reverse getLine
  putStrLn ln

r3 = do
  ln <- fmap' reverse getLine
  putStrLn ln
  ln2 <- fmap' (++ "--qwerty") getLine
  putStrLn ln2
