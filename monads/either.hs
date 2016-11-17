type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Either Pole Pole
landLeft n (l, r)
  | abs (n + l - r) > 3 = Left (n + l, r)
  | otherwise = Right (n + l, r)

landRight :: Birds -> Pole -> Either Pole Pole
landRight n (l, r)
  | abs (n + r - l) > 3 = Left (l, r + n)
  | otherwise = Right (l, r + n)

t1 = do
  let p0 = (0, 0)
  p1 <- landLeft 2 p0
  p2 <- landRight 4 p1
  p3 <- landLeft (-1) p2
  landRight (-2) p3

t2 = do
  let p0 = (0, 0)
  p1 <- landLeft 2 p0
  p2 <- landRight 4 p1
  p3 <- landLeft (-2) p2
  landRight (-2) p3

t3 = do
  let p0 = (0, 0)
  p1 <- landLeft 2 p0
  p2 <- landRight 4 p1
  Left (1000, 10)
  p3 <- landLeft (-1) p2
  landRight (-2) p3
