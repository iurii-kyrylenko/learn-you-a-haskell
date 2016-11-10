data CMaybe a = CJust Int a | CNothing deriving (Show)

-- CMaybe is not valid functor's instance
instance Functor CMaybe where
  fmap _ CNothing = CNothing
  fmap f (CJust c a) = CJust (c + 1) (f a)

t1 = fmap (*2) (CJust 100 42) -- CJust 101 84
t2 = fmap (id) (CJust 100 42) -- CJust 101 42

t3 = fmap (42:) (CJust 100 [1,2,3]) -- CJust 101 [42,1,2,3]
t4 = fmap length (CJust 100 [1,2,3]) -- CJust 101 3

t5 = fmap (length . (42:)) (CJust 100 [1,2,3]) -- CJust 101 4
t6 = (fmap length . fmap (42:)) (CJust 100 [1,2,3]) -- CJust 102 4