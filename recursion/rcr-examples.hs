rev :: [a] -> [a]
rev []=[]
rev (x:xs) = rev xs ++ [x]

zip' :: [a]->[b]->[(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq e) => e -> [e] -> Bool
elem' _ [] = False
elem' e (x:xs) = e == x || elem' e xs

elem'' :: (Eq e) => e -> [e] -> Bool
elem'' _ [] = False
elem'' e (x:xs)
  | e == x = True
  | otherwise = elem'' e xs
