module Arbitrariness where

import Test.QuickCheck
import Test.QuickCheck.Function
import Text.Show.Functions


newtype Identity a = Identity a deriving (Show, Eq)


instance Arbitrary a => Arbitrary (Identity a) where
    --arbitrary = Identity <$> arbitrary
    arbitrary =
            do
                a <- arbitrary
                return (Identity a)

data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

toList :: [a] -> List a
toList = foldr (\x l -> Cons x l) Nil

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        as <- listOf arbitrary
        return (toList as)

-- e.g.
-- *Arbitrariness> sample (arbitrary::Gen (List Int))
--
