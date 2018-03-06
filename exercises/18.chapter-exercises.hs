module MonadExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.

data Nope a =
    NopeDotJpg
    deriving (Show, Eq)

instance Eq a => EqProp (Nope a) where
    (=-=) = eq

instance Arbitrary a =>
    Arbitrary (Nope a) where
        arbitrary = pure NopeDotJpg

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return _ = NopeDotJpg
    (>>=) _ _ = NopeDotJpg

-- 2.
data PhhhbbtttEither b a =
    Left a
    | Right b
    deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (PhhhbbtttEither b a) where
        arbitrary = 
            oneof [
                MonadExercises.Left <$> arbitrary, 
                MonadExercises.Right <$> arbitrary 
            ]

-- Which is the same as:
--
-- instance (Arbitrary a, Arbitrary b) =>
--     Arbitrary (PhhhbbtttEither b a) where
--         arbitrary = do
--             a <- arbitrary
--             b <- arbitrary
--             oneof [return (MonadExercises.Left a), return (MonadExercises.Right b)]


instance Functor (PhhhbbtttEither b) where
    fmap _ (MonadExercises.Right b) = MonadExercises.Right b
    fmap f (MonadExercises.Left a) = MonadExercises.Left (f a)

-- f :: PhhhbbtttEither b
instance 
    Applicative (PhhhbbtttEither b) where
        -- Applicative f => a -> (PhhhbbtttEither b) a
        pure a = MonadExercises.Left a
        -- (<*>) :: Applicative f => f (a -> c) -> f a -> f c
        --                          PhE (a ->c) -> PhE a -> PhE c
        --                          Right b
        --                          Left (a -> c)
        (<*>) (MonadExercises.Right b) _ = MonadExercises.Right b
        (<*>) (MonadExercises.Left f) ph = fmap f ph


-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>) :: m a -> m b -> m b
-- return :: a -> m a

instance
    Monad (PhhhbbtttEither b) where
        -- return :: a -> m a
        return = pure
        -- (>>=) :: m a -> (a -> m b) -> m b
        -- (>>=) :: PhE a -> (a -> PhE c) -> PhE c
        (>>=) (MonadExercises.Right b) _ = (MonadExercises.Right b)
        (>>=) (MonadExercises.Left a) f = f a

-- 3. Write a Monad instance for Identity.

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

instance Arbitrary a =>
    Arbitrary (Identity a) where
        -- Gen Identity a = Identity fmap Gen a
        arbitrary = Identity <$> arbitrary

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure a = Identity a
    (<*>) (Identity a2b) (Identity a) = Identity (a2b a)

instance Monad Identity where
    return = pure
    -- (>>=) :: m a -> (a -> m b) -> m b
    -- (>>=) :: Identity a -> (a -> m b) -> Identity b
    (>>=) (Identity a) atoIb  = atoIb a

-- 4 List

data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance (Eq a) => EqProp (List a) where
    (=-=) = eq

-- instance Arbitrary a =>
--     Arbitrary (List a) where
--         -- Gen Identity a = Identity fmap Gen a
--         arbitrary = Identity <$> arbitrary

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Applicative List where
    pure a = Cons a Nil
    -- todo
    -- (<*>) ()


main :: IO ()
main =  do
            -- 1.
            let testTuple = (1::Int,2::Int,3::Int)
            let testNopes = NopeDotJpg::Nope (Int,Int,Int)
            -- quickBatch (functor testNopes)
            -- quickBatch (applicative testNopes)
            -- quickBatch (monad testNopes)
            
            let testEither = MonadExercises.Left testTuple:: PhhhbbtttEither String (Int, Int, Int)
            quickBatch (functor testEither)
            quickBatch (applicative testEither)
            quickBatch (monad testEither)
            let testIdentity = Identity testTuple
            quickBatch (functor testIdentity)
            quickBatch (applicative testIdentity)
            quickBatch (monad testIdentity)


    
