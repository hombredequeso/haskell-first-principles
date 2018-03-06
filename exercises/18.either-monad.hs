data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (Second b) = Second (f b)
    fmap _ (First a) = First a

instance Applicative (Sum a) where
    pure b = Second b
    -- (<*>) :: Sum a (a1 -> b) -> Sum a a1 -> Sum a b
    (<*>) (Second f) (Second b) = Second (f b)
    (<*>) _ (First a) = First a
    (<*>) (First a) _ = First a

instance Monad (Sum a) where
    -- return :: b -> Sum a b
    return = pure
    -- (>>=) :: Sum a b -> (b -> Sum a b) -> Sum a b
    (>>=) (First a) f = First a
    (>>=) (Second b) f = f b


