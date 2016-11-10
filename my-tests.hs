import Control.Exception

sut1 = (2^)
sut2 = compare 'a'

runTests :: [(String, Bool)] -> [String]
runTests = fmap (\(s, r) -> assert r s)

t = runTests
  [
    ("sut1", sut1 10 == 1024)
  , ("sut2", sut2 'A' == GT)
  , ("prd", product [1..5] == 120)
  ]
