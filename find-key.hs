-- findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
-- findKey key [] = Nothing
-- findKey key ((k,v):xs) = if key == k then Just v else findKey key xs

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key xs = foldr (\(k,v) acc -> if k == key then Just v else acc ) Nothing xs
