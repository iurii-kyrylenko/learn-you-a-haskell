quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  -- let left = quicksort [a | a <- xs, a <= x]
  --     right = quicksort [a | a <- xs, a > x]
  let left = quicksort $ filter (<= x) xs
      right = quicksort $ filter (> x) xs
  in
    left ++ [x] ++ right

-- take 10 $ quicksort [4000, 3999..1]
