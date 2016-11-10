zipWith' f x y = [f a b | (a,b) <- zip' x y]
  where
    zip' [] _ = []
    zip' _ [] = []
    zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

flip' f x y = f y x