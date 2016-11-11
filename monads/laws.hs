-- Associativity law
-- (m >>= f) >>= g
-- m >>= (\x -> f x >>= g)

f = (:[]).(*2)
g = (:[]).(+2)

a1 = [1..5] >>= f >>= g
a2 = ([1..5] >>= f) >>= g
a3 = [1..5] >>= (\x -> f x >>= g)

-- Composition
cf :: (b -> c) -> (a -> b) -> (a -> c)
f `cf` g = \x -> f (g x)
c1 = ((<0) `cf` (+(-42))) 21 

cm :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f `cm` g = \x -> (g x) >>= f
f1 x = [x, -x]
g1 x = [x*2, x^2]
c2 = return 10 >>= g1 >>= f1
c3 = return 10 >>= (f1 `cm` g1)
