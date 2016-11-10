max' :: (Ord a) => [a] -> a
max' [] = error "error"
max' [x] = x
max' (x:xs) = if x > (max' xs) then x else max' xs

max'' :: (Ord a) => [a] -> a
max'' [] = error "error"
max'' [x] = x
max'' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = max'' xs

max''' :: (Ord a) => [a] -> a
max''' [] = error "error"
max''' [x] = x
max''' (x:xs) = max x (max''' xs)
