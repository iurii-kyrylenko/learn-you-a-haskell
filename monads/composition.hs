import Control.Monad

g :: (Monad m, Num a) => a -> m a
g = return . (+1) <=< return . (*100)

t1 :: (Num a) => Maybe a
t1 = Just 4 >>= g

t2 :: (Num a) => [a]
t2 = [1,2,3,4,5] >>= g

t3 :: (Num a, Read a) => IO a
t3 = liftM read getLine >>= g

g1 :: (Monad m, Num a) => a -> m a
g1 = foldl (<=<) (return . id) [return . (+1), return . (*100)]

t4 :: (Num a, Read a) => IO a
t4 = liftM read getLine >>= g1
