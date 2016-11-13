import Control.Monad.Writer

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

-- d1 = DiffList ([1,2,3]++)
-- d2 = getDiffList d1 [42,43]

toDiffList :: [a] -> DiffList a
toDiffList = DiffList . (++)

-- d1 = toDiffList [1,2,3]
-- d2 = getDiffList d1 [42,43] 

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []
-- fromDiffList d = getDiffList d []
-- fromDiffList = flip getDiffList []

-- d1 = toDiffList [1,2,3]
-- d2 = fromDiffList d1

instance Monoid (DiffList a) where
  mempty = toDiffList []
  (DiffList f) `mappend` (DiffList g) = DiffList (f . g)

-- d1 = fromDiffList $ (toDiffList [1..5]) `mappend` (toDiffList [6..10])

-- myLog :: Int -> Writer [String] Int
-- myLog x = writer (x, ["got it: " ++ (show x)])

finalCountDown :: Int -> Writer [String] Int
finalCountDown 0 = do
  tell ["ain't get it"]
  return 0
finalCountDown x = do
  -- tell ["got it: " ++ (show x)]
  result <- finalCountDown (x - 1)
  tell ["got it: " ++ (show x)]
  return result

-- test: fc 100000
fc :: Int -> IO ()
fc = (mapM_ putStrLn) . snd . runWriter . finalCountDown

finalCountDown' :: Int -> Writer (DiffList String) Int
finalCountDown' 0 = do
  tell $ toDiffList ["ain't get it"]
  return 0
finalCountDown' x = do
  -- tell $ toDiffList ["got it: " ++ (show x)]
  result <- finalCountDown' (x - 1)
  tell $ toDiffList ["got it: " ++ (show x)]
  return result

-- fc10 :: Writer (DiffList String) Int
-- fc10 = finalCountDown' 10

-- f1 :: (Int, DiffList String)
-- f1 = runWriter fc10

-- f2 :: DiffList String
-- f2 = snd f1

-- f3 :: [String]
-- f3 = fromDiffList f2

--- test: fc' 100000
fc' :: Int -> IO ()
fc' = (mapM_ putStrLn) . fromDiffList . snd . runWriter . finalCountDown'
