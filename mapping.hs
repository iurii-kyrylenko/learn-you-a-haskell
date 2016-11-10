import Data.Char

t1 :: IO String
t1 = getLine

t2 :: [Int]
t2 = fmap ord "qwerty"

--
-- t3 = fmap ord getLine
--
-- Couldn't match type ‘[Char]’ with ‘Char’
-- Expected type: IO Char
-- Actual type: IO String

t3 :: IO Int
t3 = fmap ord getChar

t4 = fmap (fmap ord) getLine
t4' = (fmap . fmap) ord getLine

t5 :: IO [Int]
t5 = do
  line <- getLine
  return $ fmap ord line

-- :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- :t fmap ord
-- fmap ord :: Functor f => f Char -> f Int

-- t5 = getLine >> return (fmap ord "123")
t6 = getLine >>= (\line -> return $ fmap ord line)

t7 = getLine >>= return . (fmap ord)

t8 = getLine >>= pure . (ord <$>)
