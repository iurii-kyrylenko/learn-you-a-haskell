reverseWords = unwords . map reverse . words

test = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      test

test2 = do
  line <- getLine
  if null line
    then putStrLn "-- THANK YOU! --"
    else do
      putStrLn $ reverseWords line
      test2
