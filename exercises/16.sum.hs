data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)


sf = First "abc"
ss = Second 2
result1 = fmap (\x -> x + 1) sf
result2 = fmap (\x -> x + 1) ss
