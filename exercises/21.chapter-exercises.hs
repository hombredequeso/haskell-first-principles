{-# LANGUAGE FlexibleContexts #-}

module ChapterExerices21 where

-- import Prelude hiding (Either, Left, Right)
-- import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- class (Functor t, Foldable t) => Traversable (t :: * -> *) where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   sequenceA :: Applicative f => t (f a) -> f (t a)
--   mapM :: Monad m => (a -> m b) -> t a -> m (t b)
--   sequence :: Monad m => t (m a) -> m (t a)
--   {-# MINIMAL traverse | sequenceA #-}

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap a2b (Identity a) = Identity (a2b a)

instance Foldable Identity where
    foldr f bInit (Identity a) = f a bInit


instance Traversable Identity where
    -- _todo :: f (Identity b)
    -- traverse (a -> f b) (Identity a) -> f (Identity b)
    traverse a2fb (Identity a) = fmap Identity (a2fb a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

-- Constant

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
    foldr f bInit (Constant a) = bInit

-- class (Functor t, Foldable t) => Traversable (t :: * -> *) where
-- Constant a (ta) is Foldable, and Functor
--
instance Traversable (Constant a) where
--  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--  f (Constant a b)
    -- traverse a2fb ta = (fmap a2fb ta)
    -- traverse produces: f (Constant a b)
    -- f is Applicative, so can pure one up
    -- Since Constant is, well, Constant, just pass it on.
    traverse a2fb (Constant a) = pure (Constant a)

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant  a b) where
    (=-=) = eq

-- Maybe

data Optional a =
    Nada
  | Yep a
    deriving (Eq, Ord, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
    foldr ab2b b Nada = b
    foldr ab2b b (Yep a) = ab2b a b

-- e.g. if f is list: a2Listb (Optional a) -> List (Optional a)
--      f must be applicative
instance Traversable Optional where
    traverse a2fb Nada = pure Nada       -- f (Optional a)
    traverse a2fb (Yep a) = fmap Yep (a2fb a)       -- f (Optional a)


instance Arbitrary a => Arbitrary (Optional a) where
    -- oneof :: [Gen a] -> Gen a
    arbitrary = oneof [Yep <$> arbitrary, return Nada]

    -- using do form:
    -- arbitrary = do
    --                 a <- Yep <$> arbitrary
    --                 elements [Nada, a]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq

-- -- List

-- data List a =
--     Nil
--   | Cons a (List a)



idTrigger = undefined :: Identity (Int, Int, [Int])
constTrigger = undefined :: Constant Int (Int, Int, [Int])
optionalTrigger = undefined :: Optional (Int, Int, [Int])

main = do
    putStr "\nIdentity"
    quickBatch (traversable idTrigger)
    putStr "\nConstant"
    quickBatch (traversable constTrigger)

    putStr "\nOptional"
    quickBatch (traversable optionalTrigger)

