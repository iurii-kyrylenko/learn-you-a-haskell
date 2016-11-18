import Control.Monad

t1 = foldl (\a x -> a + x) 0 [1..5]

myfld :: Int -> Int -> Maybe Int
myfld a x
    | r > 42 = Nothing
    | otherwise = Just r
    where r = a + x

t2 = foldM myfld 0 [1..8]
t3 = foldM myfld 0 [1..9]


t4 = mapM (\x -> [x, '1']) "00000"

t5 = mapM_ putStrLn t4
