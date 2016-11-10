-- Customized show
data Tree a = EmptyTree | Node a (Tree a) (Tree a)
  deriving (Read, Eq)

instance (Show a) => Show (Tree a) where
  show EmptyTree = "[]"
  show (Node n l r) = "[" ++ show n ++ ", " ++ show l ++ " ," ++ show r ++ "]"

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node n l r) = Node (f n) (fmap f l) (fmap f r)

-- Derived Show
-- data Tree a = EmptyTree | Node a (Tree a) (Tree a)
--   deriving (Show, Read, Eq)

addNode :: Ord a => a -> Tree a -> Tree a
addNode node EmptyTree = Node node EmptyTree EmptyTree
addNode node (Node n l r)
  | node < n = Node n (addNode node l) r
  | node > n = Node n l (addNode node r)
  | otherwise = Node n l r

treeElem :: Ord a => a -> Tree a -> Bool
treeElem node EmptyTree = False
treeElem node (Node n l r)
  | node < n = treeElem node l
  | node > n = treeElem node r
  | otherwise = True

treeFromList :: Ord a => [a] -> Tree a
treeFromList = foldr addNode EmptyTree
