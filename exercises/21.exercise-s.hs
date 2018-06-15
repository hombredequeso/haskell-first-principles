{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
  , Arbitrary (n a)
  , Arbitrary a )
  => Arbitrary (S n a) where
      arbitrary =
          S <$> arbitrary <*> arbitrary

instance ( Applicative n
  , Testable (n Property)
  , EqProp a )
  => EqProp (S n a) where
      (S x y) =-= (S p q) =
          (property $ (=-=) <$> x <*> p)
        .&. (y =-= q)

instance Functor n => Functor (S n) where
    fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
    -- given: Monoid m
    -- foldMap is: (a -> m) -> S n a -> m
    foldMap a2m (S na a) = (a2m a) `mappend` (foldMap a2m na)


instance Traversable n
  => Traversable (S n) where
      -- _todo :: (a -> f b) -> S n a -> f (S n b)
      -- given: Applicative f
      -- traverse a2fb (S na a) = S <$> (a2fb <$> na) <*> (a2fb a)
      -- and: Traversable n
      traverse a2fb (S na a) = S <$> (traverse a2fb na) <*> (a2fb a)
      -- Since n is Traversable:
      --        traverse a2fb na -> f (N b)
      --        i.e. instead of being constrained to what fmap would give us:
      --            fmap a2fb na -> N (f b)
      --             traverse is able to flip to the N and f.
      -- for the second part: a2fb a -> f b
      -- At this point we have two f * values.
      -- It is further known that f is Applicative, so now it is a simple
      -- case of applying the standard <$> <*> <*> ... pattern.
      -- S <$> x <*> y
      --    where   x is f (N b)
      --            y is f b

main =
    sample' (arbitrary :: Gen (S [] Int))
