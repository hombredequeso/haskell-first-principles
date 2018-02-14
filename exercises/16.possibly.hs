data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

