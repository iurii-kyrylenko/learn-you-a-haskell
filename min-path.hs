-------------------------------------------
--    SOLUTION W/O USING CUSTOM TYPES    --
-------------------------------------------

process :: (Ord a, Num a) => [(a, a)] -> [a] -> [(a, a)]
process ((l,r):xs) [a,b,c] =
  let chose a l c r = a + min l (c + r)
  in (chose a l c r, chose b r c l):(l,r):xs

fold :: (Foldable t, Ord a, Num a) => t [a] -> [(a, a)]
fold = foldl process [(0,0)]

path :: (Foldable t, Ord a, Num a) => t [a] -> [Char]
path =
  let mapping (l,r) | l <= r ='A' | otherwise = 'B'
  in (map mapping) . fold

price :: (Foldable t, Ord a, Num a) => t [a] -> a
price =
  let pairMin (l,r) = min l r
  in pairMin . head . fold

-- test --
td = [[10,8,0], [40,2,25], [5,90,20], [50,10,30]]
-- "BABBA"
t1 = path td
-- 75
t2 = price td
