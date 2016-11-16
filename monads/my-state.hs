import qualified GHC.Base as B -- to use B.ap and B.liftM

newtype MyState s a = MyState { runState :: s -> (a, s) }

pop = MyState (\(x:xs) -> (x, xs))
push x = MyState (\xs -> ((), x:xs))

r1 = runState pop [1..5]
r2 = runState (push 42) [1..5]

processStack xs x =
  let ((), xs1) = runState (push x) xs
      (a, xs2) = runState pop xs1
      result = runState pop xs2
  in result

r3 = processStack [1..5] 42

processStack' (a, s) = processStack s

r4 = processStack' ((), [1..5]) 42

instance Monad (MyState s) where
  return x = MyState (\s -> (x, s))
  -- (>>=) :: MyState a -> (a -> MyState b) -> MyState b
  MyState x >>= g = MyState (\s ->
    let (a, newState) = x s
        MyState f = g a
    in f newState)

instance Applicative (MyState s) where
  pure = return
  (<*>) = B.ap

instance Functor (MyState s) where
  fmap = B.liftM

process = do
  push 42
  push 43
  a <- pop
  push 44
  push a
  push 45
  return a

t1 = runState process []
t2 = runState (push 42 >>= \_ -> push 43 >>= \_ -> pop >>= \a -> push 44 >>= \_ -> push a >>= \_ -> push 45 >> return a) [] 

