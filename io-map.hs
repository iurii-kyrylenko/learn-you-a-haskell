import Control.Monad

test = do
  colors <- forM [1,2,3,4] (\a -> do
    putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
    getLine)
    -- color <- getLine
    -- return color)
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  -- mapM putStrLn colors
  forM_ colors putStrLn
