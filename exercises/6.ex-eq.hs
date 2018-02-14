module ExEq where

data TisAnInteger =
    TisAn Integer
    deriving Show

instance Eq TisAnInteger where
    (==) (TisAn i) (TisAn i') = i == i'


data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two i0 i1) (Two i0' i1') = 
        i0 == i0' && i1 == i1' 


data StringOrInt =
    TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt i) (TisAnInt i') = i == i'
    (==) (TisAString s) (TisAString s') = s == s'
    (==) _ _ = False

data Pair a =
    Pair' a a

instance (Eq a) => Eq (Pair a) where
   (==) (Pair' m n) (Pair' m' n') = 
       (m == m') && (n == n')


data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'


data Which a =
    ThisOne a
    | ThatOne a

instance (Eq a) => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') = x == x'
    (==) (ThatOne x) (ThatOne x') = x == x'
    (==) _ _ = False


data EitherOr a b =
    Hello a
    | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye b) (Goodbye b') = b == b'
    (==) _ _ = False

-- xx::Which String
-- xx = ThisOne "abc"
-- 
-- yy:: Which Integer
-- yy = ThisOne 5
-- 
-- bb::Bool
-- bb = xx == xx




