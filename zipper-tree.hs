data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

testTree :: Tree Char
testTree = (
  Node 'A' (
    Node 'B' (
      Node 'D'
        Empty
        Empty
    ) (
      Node 'E'
        Empty
        Empty
      )
    ) (
    Node 'C' (
      Node 'F'
        Empty
        Empty
    ) (
      Node 'G' 
        Empty
        Empty
      )
    )
  )

smallTree = (
  Node 'a' (
    Node 'b'
      Empty
      Empty
    ) (
    Node 'c' 
      Empty
      Empty
    )
  )

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

moveLeft :: Zipper a -> Zipper a
moveLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

moveRight :: Zipper a -> Zipper a
moveRight (Node x l r, bs) = (r, RightCrumb x l:bs)

moveUp :: Zipper a -> Zipper a
moveUp (l, (LeftCrumb x r):bs) = (Node x l r, bs)
moveUp (r, (RightCrumb x l):bs) = (Node x l r, bs)

update :: a -> Zipper a -> Zipper a
update x z@(Empty, bs) = z
update x (Node _ l r, bs) = (Node x l r, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

top :: Zipper a -> Zipper a
top z@(_, []) = z
top z = top $ moveUp z

(-:) :: a -> (a -> b) -> b
x -: f = f x

z1 = (testTree, []) -: moveLeft -: moveRight -: update '*'
z2 = z1 -: moveLeft -: update 'X'
z3 = z2 -: attach smallTree
t3 = z3 -: top -: fst

