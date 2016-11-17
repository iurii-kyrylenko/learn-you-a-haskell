import Control.Monad.State

push' :: a -> State [a] ()
push' x = state $ \xs -> ((), x:xs)

pop' :: State [a] a
pop' = state $ \(x:xs) -> (x, xs)

t1 = runState (push' 42) [1..3]
t2 = runState (state $ \s -> ((), 1:2:s)) [1..3]


t3 = runState (state $ \s -> ((push' 10), 1:2:s)) [1..3]
t3' = runState (join (state $ \s -> ((push' 10), 1:2:s))) [1..3]

-- m >>= f always equals join (fmap f m)
t4 = join $ fmap putStrLn getLine
t5 = getLine >>= putStrLn
