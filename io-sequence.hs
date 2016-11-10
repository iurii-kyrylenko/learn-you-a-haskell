myseq [] = return ()
myseq (x:xs) = x >> (myseq xs)

myseq' = foldl1 (>>)

myseq2 [] = return ()
myseq2 (x:xs) = do
  a <- x
  b <- myseq2 xs
  return ()

test = myseq $ map print [1,2,3]
test' = myseq' $ map print [1,2,3]
test2 = myseq2 $ map print [1,2,3]
