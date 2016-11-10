import Data.Char
import qualified Data.Text as T
import Control.Exception

--------------------------------------
-- Low #1: fmap id = id
-- Low #2: (fmap f) . (fmap g) = fmap (f . g)
--------------------------------------

strippedString :: String -> Maybe String
strippedString s =
  let ss = (T.unpack . T.strip . T.pack) s
  in if ss == [] then Nothing else Just ss

test =
  let actual = fmap strippedString ["  qwerty  ", "   ", "", "asdfg"];
      expected = [Just "qwerty", Nothing, Nothing, Just "asdfg"]
  in  assert (actual == expected) 42

t1 :: IO Int
t1 = fmap ord getChar

t2 :: IO [Int]
t2 = fmap (fmap ord) getLine
--t2' = (fmap.fmap) ord getLine

t3 :: IO (Maybe String)
t3 = fmap (fmap reverse) (fmap strippedString getLine)
-- t3' = getLine >>= return . strippedString . reverse

t4 :: IO (Maybe [Int])
t4 = fmap (fmap (fmap ord)) (fmap strippedString getLine)

t5 :: IO (Maybe [Int])
t5 = fmap (fmap (fmap ord)) (fmap (fmap reverse) (fmap strippedString getLine))
-- t5' = (fmap.fmap.fmap) ord ((fmap.fmap) reverse (fmap strippedString getLine))

