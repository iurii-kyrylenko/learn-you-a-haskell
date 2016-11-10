import Control.Applicative

f1 :: Num a => Maybe (a -> a)
f1 = Just (3*)
f2 = Just 5
-- want to get Just 15
--f2 = fmap (.(1+)) f1
f3 = f1 <*> f2

t1 :: Num a => [a -> a]
t1 = [(+1), (+2), (*3)]
t2 = [42, 43, 44]
-- want to get [43, 44, 45, 44, 45, 46, 126, 129, 132]
t3 = t1 <*> t2

a1 :: IO String
a1 = do
  a <- getLine
  b <- getLine
  return $ a ++ b

a2 = fmap (++) getLine <*> getLine

a3 = (++) <$> getLine <*> getLine

f4 = (+) <$> (+3) <*> (*100)

la2 = liftA2 (,,) [1..6] "qwerty"
t4 = la2 <*> [True, False]

la2' = (,,) <$> [1..6] <*> "qwerty"
t4' = la2 <*> [True, False]

t5 = compare <$> getLine <*> getLine

-- l1 = liftA2 (:) (Just 2) (pure []) -- Just [2]
-- l2 = liftA2 (:) (Just 3) l1 -- Just [3,2]

l0 = pure [] :: Maybe [Int] -- Just []
l1 = (:) <$> Just 1 <*> l0  -- Just [1]
l2 = (:) <$> Just 2 <*> l1  -- Just [2,1]

seqA :: (Applicative f) => [f a] -> f [a]
seqA = foldr (liftA2 (:)) (pure [])
-- seqA [] = pure []
-- seqA (fx : fxs) = (:) <$> fx <*> (seqA fxs)

s1 :: Maybe [Int]
s1 = seqA [Just 1, Just 2, Just 3] -- Just [1,2,3]
s1' :: Maybe [Int]
s1' = seqA [Just 1, Nothing, Just 3] -- Nothing

s2 :: IO [String]
s2 = seqA [getLine, getLine, getLine]

s3 :: [[Int]]
s3 = seqA [[1,2],[3,4]] -- [[1,3],[1,4],[2,3],[2,4]]

s4 :: [[Int]]
s4 = getZipList $ seqA [ZipList[1,2],ZipList[3,4]] -- [[1,3],[2,4]]

-- s5 :: Bool
s5 = and $ seqA [(>4),(<10),odd] 42
