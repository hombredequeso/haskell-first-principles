module ApplicativeExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
-- import Control.Applicative
-- import Data.Monoid

-- 1. Pair

-- WARNING: don't forget to have Eq here.
-- If you don't, then attempting to declare "EqProp (Pair a)"
-- will result in this error:
--
-- 17.exercises.hs:17:13: error:                                                   
--     * Could not deduce (Eq (Pair a)) arising from a use of `eq'                 
--       from the context: Eq a                                                    
--         bound by the instance declaration at 17.exercises.hs:16:10-32           
data Pair a = Pair a a deriving (Show, Eq)

instance Eq a => EqProp (Pair a) where 
    (=-=) = eq

-- instance Arbitrary a =>
--     Arbitrary (Pair a) where
--         arbitrary = do
--                         a1 <- arbitrary
--                         a2 <- arbitrary
--                         return (Pair a1 a2)

-- or better:
-- because Pair <$> arbitrary
--          a -> a -> Pair a 
--                  Gen a
--                          -> Gen (a -> Pair a)
instance Arbitrary a =>
    Arbitrary (Pair a) where
        arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor Pair where
    fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
    pure x = Pair x x
    (<*>) (Pair f1 f2) (Pair a1 a2) = Pair (f1 a1) (f2 a2)


-- 2. Data 2

data Two a b = Two a b deriving (Show, Eq)

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Two a b) where
        arbitrary = Two <$> arbitrary <*> arbitrary

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a =>
    Applicative (Two a) where
        pure = Two mempty 
        (<*>) (Two a1 f) (Two a2 b) = Two (mappend a1 a2) (f b)

-- Note how it is an Applicative of (Two a), which means that
-- (Two a) is the structural part, and is preserved.


-- 3. data Three a b c = Three a b c

data Three a b c = Three a b c deriving (Show, Eq)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
        arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) =>
    Applicative (Three a b) where
        pure = Three mempty mempty
        (<*>) (Three a1 b1 f) (Three a2 b2 c) = 
            Three (mappend a1 a2) (mappend b1 b2) (f c)

-- 4. data Three' a b = Three' a b b

data Three' a b = Three' a b b deriving (Show, Eq)

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Three' a b) where
        arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Monoid a) =>
    Applicative (Three' a) where
        pure b = Three' mempty b b
        (<*>) (Three' a1 fb1 fb2) (Three' a2 b1 b2) = 
            Three' (mappend a1 a2) (fb1 b1) (fb2 b2)


-- 5. data Four a b c d = Four a b c d

data Four a b c d = Four a b c d deriving (Show, Eq)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
        arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) =>
    Applicative (Four a b c) where
        pure = Four mempty mempty mempty
        (<*>) (Four a1 b1 c1 f) (Four a2 b2 c2 d) = 
            Four (mappend a1 a2) (mappend b1 b2) (mappend c1 c2) (f d)

-- 6. data Four' a b = Four' a a a b

data Four' a b = Four' a a a b deriving (Show, Eq)

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Four' a b) where
        arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Four' a ) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Monoid a) =>
    Applicative (Four' a) where
        pure = Four' mempty mempty mempty
        (<*>) (Four' a1 a2 a3 f) (Four' a1' a2' a3' b) = 
            Four' (mappend a1 a1') (mappend a2 a2') (mappend a3 a3') (f b)

main :: IO ()
main =  do
            let testTuple = (1::Int,2::Int,3::Int)
            let testPair = Pair (1::Int,2::Int,3::Int) (4,5,6)
            quickBatch (functor testPair)
            quickBatch (applicative testPair)

            let  testTwo = Two "ZZZZ" testTuple
            quickBatch (functor testTwo)
            quickBatch (applicative testTwo)

            let testThree =  Three "AAAA" "BBBBB" testTuple
            quickBatch (functor testThree)
            quickBatch (applicative testThree)

            let testThree' =  Three' "AAAA" testTuple testTuple
            quickBatch (functor testThree')
            quickBatch (applicative testThree')

            let testFour =  Four "AAAA" "BBBBB" "CCCC" testTuple
            quickBatch (functor testFour)
            quickBatch (applicative testFour)

            let testFour' =  Four' "AAAA" "BBBBB" "CCCC" testTuple
            quickBatch (functor testFour')
            quickBatch (applicative testFour')


