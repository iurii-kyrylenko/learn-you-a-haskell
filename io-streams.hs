-- import Data.Char

-- main = do
--   content <- getContents
--   putStrLn $ map toUpper content

-- main = do
--   content <- getContents
--   putStr $ shortLinesOnly content

-- shortLinesOnly input =
--   let allLines = lines input
--       isStringShort = (<10) . length
--       shortLines = filter isStringShort allLines
--       result = unlines shortLines
--   in result

-- main = interact' $ unlines . filter ((<25) . length) . lines

-- interact' convert = do
--   content <- getContents
--   putStr $ convert content  

main = interact $ unlines . map isPalindrome . lines

isPalindrome line
  | line == reverse line = "palindrome"
  | otherwise = "not a palindrome"
