import System.Environment
import System.IO
import System.IO.Error
import Control.Exception

main = tryBlock `catch` handler

tryBlock :: IO ()
tryBlock = do
  content <- readFile "/Users/iurii_kyrylenko/wrk/wp-boot.txt_"
  putStrLn content

handler :: IOError -> IO ()
handler err = do
  putStrLn "Got an error!"
  putStrLn $ displayException err
  putStrLn $ show $ ioeGetFileName err
  -- ioError err
  -- throw err
  throwIO DivideByZero
