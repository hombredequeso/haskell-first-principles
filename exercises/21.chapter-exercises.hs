{-# LANGUAGE FlexibleContexts #-}

module ChapterExerices21 where

import Control.Applicative
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

-- List

data List a =
    Nil
  | Cons a (List a)
    deriving (Eq, Ord, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)


-- foldr : a doesn't have to be a monoid,
--          but it is necessary to provide and initial b value
--          and a function like \a b -> b
instance Foldable List where
    foldr ab2b bInit Nil = bInit
    foldr ab2b bInit (Cons a as) = ab2b a ( foldr ab2b bInit as)


-- foldMap : a has to  be a monoid.
--          That takes care of the initial value, and how to combine them.
--
-- instance Foldable List where
--     foldMap _ Nil = mempty
--     foldMap a2m (Cons head tail) = mappend (a2m head) (foldMap a2m tail)

instance Traversable List where
    -- (a -> f b) -> List a -> f (List b)
    -- and f is applicative
        -- e.g. if f was Maybe...
        --  (a -> Maybe b) -> List a -> Maybe (List b)
        --  (a -> Maybe b) -> Maybe (List b) ,   Maybe (List b) -> Maybe b -> Maybe (List b)
    -- traverse a2fb as = _todo
    traverse _ Nil = pure Nil
    traverse a2fb (Cons a as) = liftA2 Cons (a2fb a) (traverse a2fb as)
    -- which is the same as:
    -- traverse a2fb (Cons a as) =  fmap Cons (a2fb a) <*> (traverse a2fb as)

toList :: [a] -> List a
toList = foldr Cons Nil

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = fmap toList (listOf arbitrary)

instance Eq a => EqProp (List a) where
    (=-=) = eq

-- Three
data Three a b c =
    Three a b c
    deriving (Eq, Ord, Show)

instance Traversable (Three a b) where
    -- (c -> f d) -> Three a b c -> f (Three a b d)
    -- f is applicative
    traverse c2fd (Three a b c) = Three <$> (pure a) <*> (pure b) <*> (c2fd c)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldr ab2b bInit (Three m n a) = ab2b a bInit

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance ( Eq a, Eq b, Eq c ) => EqProp (Three a b c) where
    (=-=) = eq


-- Pair
data Pair a b =
    Pair a b
    deriving (Eq, Ord, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
    foldr f cInit (Pair a b) = f b cInit

instance Traversable (Pair a) where
    -- (b -> f c) -> Pair a b -> f (Pair a c)
    -- Applicative f
    traverse b2fc (Pair a b) = Pair <$> (pure a) <*> (b2fc b)
    -- or:
    -- traverse b2fc (Pair a b) = (pure Pair) <*> (pure a) <*> (b2fc b)
    -- traverse b2fc (Pair a b) = (pure ( Pair a )) <*> (b2fc b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
    (=-=) = eq

-- Big 

data Big a b =
    Big a b b
    deriving (Eq, Ord, Show)

instance Functor (Big a) where
    fmap f (Big a b1 b2) = Big a (f b1) (f b2)

-- Use foldMap here, because it requires that the result (of type m)
-- be a monoid, hence it is possible to `mappend` the two parts together.
instance Foldable (Big a) where
    foldMap b2m (Big a b1 b2) = (b2m b1) `mappend` (b2m b2)

instance Traversable (Big a)  where
    -- (b -> f c) -> Big a b -> f (Big a c)
    -- f is applicative
    traverse b2fc (Big a b1 b2) = (Big a) <*> (b2fc b1) <*> (b2fc b2)

instance ( Arbitrary a, Arbitrary b ) => Arbitrary (Big a b) where
    arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance ( Eq a, Eq b ) => EqProp (Big a b) where
    (=-=) = eq

idTrigger = undefined :: Identity (Int, Int, [Int])
constTrigger = undefined :: Constant Int (Int, Int, [Int])
optionalTrigger = undefined :: Optional (Int, Int, [Int])
listTrigger = undefined :: List (Int, Int, [Int])
threeTrigger = undefined :: Three Int Int (Int, Int, [Int])
pairTrigger = undefined :: Pair Int (Int, Int, [Int])
bigTrigger = undefined :: Big Int (Int, Int, [Int])

main = do
    putStr "\nIdentity"
    quickBatch (traversable idTrigger)
    putStr "\nConstant"
    quickBatch (traversable constTrigger)

    putStr "\nOptional"
    quickBatch (traversable optionalTrigger)

    putStr "\nList"
    quickBatch (traversable listTrigger)

    putStr "\nThree"
    quickBatch (traversable threeTrigger)

    putStr "\nPair"
    quickBatch (traversable pairTrigger)

    putStr "\nBig"
    quickBatch (traversable bigTrigger)

