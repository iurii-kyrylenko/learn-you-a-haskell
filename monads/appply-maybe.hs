applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

t1 p = applyMaybe
    (Just p)
    (\x -> if x < 42 then Just (x + 1) else Nothing)

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
t2 p =
  (Just p)
  >>=
  (\x -> if x < 42 then return (x + 1) else Nothing)

t3 = getLine >>= return . (++ "!")

