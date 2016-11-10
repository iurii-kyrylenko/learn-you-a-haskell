import Control.Exception
import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare s1 s2 =
  let vowels = length . filter (`elem` "aeouAEOU") in
    (length s1 `compare` length s2) `mappend`
    (vowels s1 `compare` vowels s2) `mappend`
    (s1 `compare` s2)

t1 = lengthCompare "qweq" "qwae"

-----------------------------------------
t2 = ("qwerty", Sum(123)) `mappend` ("_asdfg", Sum(456))
t3 = getLine `mappend` getLine
t4 = mconcat [getLine, getLine]
-----------------------------------------

data Tree a = EmptyTree | Node a (Tree a) (Tree a)
  deriving (Read, Show, Eq)

instance Foldable Tree where
  foldMap _ EmptyTree = mempty
  foldMap f (Node a l r) =
    (f a)
    `mappend` (foldMap f l)
    `mappend` (foldMap f r)

testTree = Node 5
  (Node 3
    (Node 1 EmptyTree EmptyTree)
    (Node 6 EmptyTree  EmptyTree)
  )
  (Node 9
    (Node 8 EmptyTree EmptyTree)
    (Node 10 EmptyTree EmptyTree)
  )

test1 = assert (foldr1 (+) testTree == 42) "passed"
test2 = assert (foldr (\x acc -> acc || x == 3) False testTree == True) "passed"
-- foldMap :: (a -> m) -> t a -> m
test3 = foldMap Sum testTree
test4 = foldMap (\x -> Any (x == 3)) testTree
test5 = foldMap (Any.(==3)) testTree
test6 = foldMap (:[]) testTree
