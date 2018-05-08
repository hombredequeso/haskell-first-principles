{-# LANGUAGE ViewPatterns #-}

module Ex_16_10 where

import Test.QuickCheck
import Test.QuickCheck.Function
-- import Text.Show.Functions

--------------------------------------------------
-- Identity

newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary


--------------------------------------------------
-- Pair

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

--------------------------------------------------
-- Alternative implementations of Arbitrary (Pair a)
-- Same as above, but using prefix notation 

-- instance Arbitrary a => Arbitrary (Pair a) where
--     arbitrary = (<*>)( fmap Pair arbitrary ) arbitrary
 
-- Using do notation

-- instance Arbitrary a => Arbitrary (Pair a) where
--     arbitrary -- :: Gen (Pair a) 
--         = do
--             a1 <- arbitrary
--             a2 <- arbitrary
--             return $ Pair a1 a2
    
--------------------------------------------------
-- Two

data Two a b = Two a b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)


--------------------------------------------------
-- Three

data Three a b c = Three a b c deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

--------------------------------------------------
-- Three'

data Three' a b = Three' a b b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)


--------------------------------------------------
-- Four'
data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

--------------------------------------------------
-- Functor law tests
-- Identity
functorIdentity :: (Functor f, Eq (f a)) =>
    f a
    -> Bool
functorIdentity fa =
    fmap id fa == fa

-- Composability
functorCompose :: (Eq (f c), Functor f) =>
    (a -> b)
    -> (b -> c)
    -> f a
    -> Bool
functorCompose f g x =
    (fmap g (fmap f x)) == (fmap (g . f) x)

-- Composability, using an Arbitrary provided function
functorCompose' :: (Eq (f c), Functor f) =>
    f a
    -> Fun a b
    -> Fun b c
    -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

-- type declaration for use with Arbitraries in functorCompose'
type IntToInt = Fun Int Int

main :: IO ()
main = do

    -- Identity Tests
    quickCheck (functorIdentity :: Identity Int -> Bool)

    let c = functorCompose (+1) (*2)
    let li x = c (x :: Identity Int)
    quickCheck li

    quickCheck (functorCompose' :: (Identity Int) -> IntToInt -> IntToInt -> Bool)


    -- Pair Tests
    quickCheck (functorIdentity :: (Pair Int) -> Bool)
    quickCheck (functorCompose' :: (Pair Int) -> IntToInt -> IntToInt -> Bool)

    -- Two Tests
    quickCheck (functorIdentity :: (Two Char Int) -> Bool)
    quickCheck (functorCompose' :: (Two Char Int) -> IntToInt -> IntToInt -> Bool)

    -- Three Tests
    quickCheck (functorIdentity :: (Three Char String Int) -> Bool)
    quickCheck (functorCompose' :: (Three Char String Int) -> IntToInt -> IntToInt -> Bool)

    -- Three' Tests
    quickCheck (functorIdentity :: (Three' Char Int) -> Bool)
    quickCheck (functorCompose' :: (Three' Char Int) -> IntToInt -> IntToInt -> Bool)

    -- Four Tests
    quickCheck (functorIdentity :: (Four' Char Int) -> Bool)
    quickCheck (functorCompose' :: (Four' Char Int) -> IntToInt -> IntToInt -> Bool)
    putStrLn "done"
