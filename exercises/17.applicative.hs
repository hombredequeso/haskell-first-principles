-- {-# LANGUAGE FlexibleInstances #-}

module ApplicativeCh17 where

import Control.Applicative

f x =
    lookup x [ (3, "hello")
                , (4, "julie")
                , (5, "kbai")]

g y =
    lookup y [ (7, "sup?")
                , (8, "chris")
                , (9, "aloha")]

h z =
    lookup z [(2, 3), (5, 6), (7, 8)]

m x =
    lookup x [(4, 10), (8, 13), (1, 9001)]


-- Identity =========================================

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure a = Identity a
    (<*>) (Identity f) (Identity a) = Identity (f a)


-- Constant =========================================

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

-- The functor applies the function to the b (phantom value)
-- Since the Functor is a functor of (Constant a), all the the
-- (Constant a) is preserved, and applying f to the phantom b
-- value means that the result is the same as the input (Constant a)

instance Functor (Constant a) where
    fmap _ (Constant a) = (Constant a)

instance Monoid a
        => Applicative (Constant a) where
            -- Given a b value lift it into the structure of (Constant a)
            -- Having only been given a 'b' value, the 'a' of (Constant a)
            -- must come from somewhere. Since 'a' has (Monoid a) we can
            -- just use Monoid a's pure
            -- In other words, lift the b value into Constant a by
            -- throwing it away and returning a (Constant a)
            pure b = Constant mempty
            (<*>) (Constant f) (Constant a) = (Constant a)


-- Exercise - Fix Upper
--
    -- 1. const <$> Just "Hello" <*> "World"
    -- 
    -- 2. (,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3]
    -- *ApplicativeCh17> const <$> Just "Hello" <*> Just "World"
    -- Just "Hello"
    -- 
    -- *ApplicativeCh17> (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]
    -- Just (90,10,"Tierness",[1,2,3])
