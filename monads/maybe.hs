type Birds = Int
type Pole = (Birds, Birds)

-- landLeft :: Birds -> Pole -> Pole
-- landLeft n (l, r) = (l + n, r)

-- landRight :: Birds -> Pole -> Pole
-- landRight n (l, r) = (l, r + n)

-- (-:) :: a -> (a -> b) -> b
-- x -: f = f x

-- t1 = (0, 0) -: landLeft 2 -: landRight 4 -: landLeft (-1) -: landRight (-2)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l, r)
  | abs (l + n - r) > 3 = Nothing
  | otherwise = Just (l + n, r)

landRight :: Birds -> Pole -> Maybe Pole
landRight n (l, r)
  | abs (r + n - l) > 3 = Nothing
  | otherwise = Just (l, r + n)

banana :: Pole -> Maybe Pole
banana _ = Nothing

t1  =   Just (0, 0) >>= landLeft 2 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
t1' = return (0, 0) >>= landLeft 2 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)

t2  =   Just (0, 0) >>= landLeft 2 >>= landRight 4 >>= landLeft (-2) >>= landRight (-2)
t2' = return (0, 0) >>= landLeft 2 >>= landRight 4 >>= landLeft (-2) >>= landRight (-2)

t3  = return (0, 0) >>= landLeft 2 >>= landRight 4 >>= banana >>= landLeft (-1) >>= landRight (-2)
t3' = return (0, 0) >>= landLeft 2 >>= landRight 4 >> Nothing >>= landLeft (-1) >>= landRight (-2)

-- return a :: (Monad m) => a -> m a
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-- (>>) :: (Monad m) => m a -> m b -> m b
-- x >> y = x >>= (\_ -> y)

a1 = Just 3 >>= (\x -> Just "!" >>= (\y -> Just $ show x ++ y))

a1' =
  Just 3
    >>= (\
      x -> Just "!"
        >>= (\
          y -> Just $ show x ++ y
          )
        )

a1'' =
  Just 3
  >>= \x ->
  Just "!"
  >>= \y ->
  Just $ show x ++ y

a1''' = do
  x <- Just 3
  y <- Just "!"
  Just $ show x ++ y

t4 = do
  let p0 = (0, 0)
  p1 <- landLeft 2 p0
  p2 <- landRight 4 p1
  p3 <- landLeft (-1) p2
  landRight (-2) p3

m = Just 9 >>= (\x -> Just (x > 8))

m1 :: Maybe Bool
m1 = do
  x <- Just 9
  Just (x > 8)

t5 = do
  p0 <- return (0, 0)
  p1 <- landLeft 2 p0
  p2 <- landRight 4 p1
  Nothing
  p3 <- landLeft (-1) p2
  landRight (-2) p3

h :: Maybe Char
h = do
  (x:_) <- Just "hello"
  -- Nothing
  return x

h1 = Just "hello" {-- >> Nothing --} >>= (\(x:_) -> Just x)
h2 = Just "hello" >>= (\xs -> (Just . head) xs)
h3 = Just "hello" >>= (Just . head)
h4 = Just "hello" >>= (return . head)

i = getLine >>= (return . head)

h5 = do
  (x:_) <- Just ""
  return x

