-- http://stackoverflow.com/questions/32929252/can-ghc-derive-functor-and-applicative-instances-for-a-monad-transformer
-- http://stackoverflow.com/questions/11684321/how-to-play-with-control-monad-writer-in-haskell

newtype MyWriter w a = MyWriter { runWriter :: (a, w) }

instance (Monoid w) => Monad (MyWriter w) where
  return x = MyWriter (x, mempty)
  (MyWriter (x, v)) >>= f =
    let (MyWriter (y, v1)) = f x
    in  MyWriter (y, v `mappend` v1)

instance (Monoid w) => Applicative (MyWriter w) where
  -- pure :: a -> f a
  pure = return
  -- (<*>) :: f (a -> b) -> f a -> f b
  -- (<*>) :: MyWriter w (a -> b) -> MyWriter w a -> MyWriter w b
  (MyWriter (f, v1)) <*> (MyWriter (x, v2)) = MyWriter (f x, v1 `mappend` v2)

instance Functor (MyWriter w) where
  -- fmap :: (a -> b) f a -> f b
  -- fmap :: (a -> b) MyWriter w a -> MyWriter w b
  fmap f (MyWriter (x, v)) = MyWriter (f x, v)

logNumber x = MyWriter (x, ["Got number: " ++ (show x)])

t1 = runWriter $
  logNumber 42 >>=
  \x -> logNumber (x + 1) >>=
  \x -> logNumber (x * 2)

t2 = runWriter $
  logNumber 42 >>=
  \x -> logNumber (x + 1) >>
  logNumber (777)

t3 = runWriter $ (+) <$> logNumber 42 <*> logNumber 43 >>= \x -> logNumber (2 * x)
