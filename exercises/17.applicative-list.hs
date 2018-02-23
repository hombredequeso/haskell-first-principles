module ApplicativeList where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)



append:: List a -> List a -> List a
append Nil l2 = l2
append (Cons h t) l2 = Cons h (append t l2)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Applicative List where
    pure x = Cons x Nil
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil
    (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)

toList :: [a] -> List a
toList = foldr (\x l -> Cons x l) Nil

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        as <- listOf arbitrary
        return (toList as)

instance Eq a => EqProp (List a) where 
    (=-=) = eq

lst :: List Int
lst = Cons 1 Nil

lst2 = Cons (1::Int,2::Int,3::Int) Nil


main :: IO ()
main = quickBatch (applicative lst2)

