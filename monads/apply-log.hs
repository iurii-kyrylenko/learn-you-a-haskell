import Data.Monoid

----------

applyLog' :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog' (x, l) f =
  let (x1, l1) = f x
  in  (x1, l ++ ":" ++ l1)

res = applyLog' (42, "qwerty") (\x -> (x+1, "added 1 to " ++ show x))

----------

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, l) f =
  let (x1, l1) = f x
  in  (x1, l `mappend` l1)

res1 = applyLog (42, "qwerty") (\x -> (x+1, "added 1 to " ++ show x))
res2 = applyLog (42, ["qwerty"]) (\x -> (x+1, ["added 1 to " ++ show x]))
res3 = applyLog ("qwerty", Sum 42) (\x -> (x ++ "!", Sum 1))

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

res4 = applyLog ("jerky", Sum 42) addDrink
res5 = res4 `applyLog`  addDrink `applyLog` addDrink

----------

