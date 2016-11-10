import qualified Data.Map as Map
data LockerState = Free | Taken deriving (Show, Eq)
type Code = String
type LockersMap = Map.Map Integer (LockerState, Code)

lockerLookup :: Integer -> LockersMap -> Either String Code
-- lockerLookup :: Integer -> LockersMap -> Maybe (LockerState, Code)

lockerLookup number lockers =
  case Map.lookup number lockers of
    Nothing -> 
      Left ("Locker #" ++ show number ++ " does not exist.")
    Just (state, code) ->
      if state == Free
        then Right code
        else Left ("Locker #" ++ show number ++ " has been taken.")

lockers = Map.fromList
  [(100,(Taken,"ZD39I"))
  ,(101,(Free,"JAH3I"))
  ,(103,(Free,"IQSA9"))
  ,(105,(Free,"QOTSA"))
  ,(109,(Taken,"893JJ"))
  ,(110,(Taken,"99292"))
  ]
