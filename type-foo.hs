 -- Tofu :: (* -> (* -> *) -> *) -> Constraint
class Tofu t where
  tofu :: j a -> t a j

-- Frank :: * -> (* -> *) -> *
data Frank a b = Frank (b a) deriving (Show)
-- data Frank a b = Frank {frankField :: b a} deriving (Show)


-- fr42 :: (Num a) => Frank a Maybe
-- fr42 = Frank (Just 42)

-- qw :: Frank Char []
-- frqw = Frank "qwerty"

instance Tofu Frank where
  tofu x = Frank x

-- qw = tofu "qwerty" :: Frank Char []

-- Barry :: (* -> *) -> * -> * -> *
data Barry t k p = Barry { yabba :: p, dabba :: t k } deriving (Show)

-- x1 :: Barry [] Char Integer
x1 = Barry { yabba = 42, dabba = "qwerty" }

-- x2 :: Barry Maybe Integer Bool
x2 = Barry { yabba = True, dabba = Just 42}

instance Functor (Barry x y) where
  fmap f (Barry { yabba = p, dabba = q }) = Barry { yabba = f p, dabba = q }

f1 = fmap (>40) x1
