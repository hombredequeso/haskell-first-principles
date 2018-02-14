
newtype Goats =
    Goats Int deriving (Eq, Show)
newtype Cows =
    Cows Int deriving (Eq, Show)


tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany Goats where
    tooMany (Goats n) = n > 43

type MyT = (Int, String)

instance TooMany MyT where
    tooMany (i, s) = i > 42

-- newtype IntStr =
--    IntStr (Int, String) deriving (Eq, Show)

-- instance TooMany IntStr where
--    tooMany (IntStr (i, s)) = i > 42
