chain 1 = [1]
-- chain n = (next):(chain next)
--   where next = if (even n) then (div n 2) else (3 * n + 1)

chain n
  | even n = n:(chain $ div n 2)
  | otherwise = n:(chain $ 3 * n + 1)

-- chainStats threshold size = length $ filter (>threshold) $ map length $ map chain [1..size]

-- chainStats threshold size = length $ filter isLong $ map chain [1..size]
--   where isLong xs = length xs > threshold

chainStats threshold size = length $ filter (\xs->length xs > threshold) $ map chain [1..size]
