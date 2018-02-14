-- {-# LANGUAGE DeriveGeneric #-}

module Ex15Semigroup where

import Data.Semigroup
import Test.QuickCheck
import Text.Show.Functions

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) 
    => m 
    -> m 
    -> m 
    -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- /////////////////////////////////////////////////////////////

newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity a' = Identity (a <> a') 


type IdentityAssoc  = Identity String -> Identity String -> Identity String -> Bool

instance (Arbitrary a, Num a) => Arbitrary (Sum a) where
    arbitrary = do
        x <- arbitrary
        return (Sum x)


-- Why doesn't this work??????
-- type MyIdSumInt a = Identity (Sum a)
-- instance Arbitrary a =>  Arbitrary (MyIdSumInt a) where
--     arbitrary = do
--         x <- arbitrary
--         return (Identity (Sum x))

type IdentityAssoc2  = Identity (Sum Integer) -> Identity (Sum Integer) -> Identity (Sum Integer) -> Bool



-- /////////////////////////////////////////////////////////////

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Two x y)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two m n <> Two p q = Two (m<>p) (n<>q)

--type TwoAssoc a b = Two a b  -> Two a b -> Two a b -> Bool
type TwoAssoc = Two String (Sum Integer) -> Two String (Sum Integer) -> Two String (Sum Integer) -> Bool

-- /////////////////////////////////////////////////////////////


newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj False <> _ = BoolConj False
    _ <> BoolConj False = BoolConj False
    _ <> _ = BoolConj True

instance Arbitrary BoolConj where
    arbitrary = do
        x <- arbitrary
        return $ BoolConj x


type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- /////////////////////////////////////////////////////////////

data Or a b  =
    Fst a
    | Snd b
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Fst a, return $ Snd b]

instance Semigroup (Or a b) where
    x@(Snd _) <> y  = x
    _ <> y@(Snd _)  = y
    x <> _ = x

type OrAssoc = Or String (Sum Integer) -> Or String (Sum Integer) -> Or String (Sum Integer) -> Bool

-- /////////////////////////////////////////////////////////////

newtype Combine a b = Combine { unCombine :: (a -> b) } 

instance Show (Combine a b) where
    show (Combine f) = show f

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- (arbitrary :: (CoArbitrary a, Arbitrary b) => Gen (a -> b))
    return $ Combine f

type CombineAssoc = Combine String (Sum Integer) -> Combine String (Sum Integer) -> Combine String (Sum Integer) -> Bool
-- combineAssoc :: Combine Int String -> Combine Int String -> Combine Int String -> Int -> Bool
-- combineAssoc a b c x = unCombine (a <> (b <> c)) x == unCombine ((a <> b) <> c ) x
-- /////////////////////////////////////////////////////////////

newtype Comp a = 
    Comp { unComp :: (a -> a) }

instance Show (Comp a) where
    show (Comp f) = show f

-- implementation:
--  if a is a semigroup, then apply each comp function separately, and semigroup the results together.
--  e.g. 
--      let g = Comp $ \n -> n <> Sum(2)
--      let f = Comp $ \n -> n <> Sum(1)
--      unComp (f <> g) $ 10
--
    -- instance Semigroup a => Semigroup (Comp a) where
    -- (Comp f) <> (Comp g) = Comp (\x -> f x <> g x)

-- Alternative #2
--      Simply compose the two functions together, and now a does not have to be a Semigroup

--
    -- *Ex15Semigroup> let f = Comp (\x -> x + 1)
    -- *Ex15Semigroup> let g = Comp (\x -> x + 2)
    -- *Ex15Semigroup> unComp (f <> g) $ 10
    
instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp (f . g)

-- /////////////////////////////////////////////////////////////
data Validation a b =
    Failure' a | Success' b
    deriving (Eq, Show)

-- Get the first success (b), otherwise semigroup the failures (a)
instance Semigroup a =>
    Semigroup (Validation a b) where
        (<>) (Failure' a1) (Failure' a2) = Failure' (a1 <> a2)
        (<>) s@(Success' _) _ = s
        (<>) (Failure' _) s@(Success' _) = s


-- /////////////////////////////////////////////////////////////
-- Looks like state to me...
--

-- newtype Mem s a =
--     Mem {
--         runMem :: s -> (a,s)
--         }
-- 
-- instance Monoid a => Monoid (Mem s a) where
--     mempty = Mem (\s -> (mempty,s))
--     mappend = _e

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdentityAssoc )
    quickCheck (semigroupAssoc :: IdentityAssoc2 )
    quickCheck (semigroupAssoc :: TwoAssoc )
    quickCheck (semigroupAssoc :: BoolConjAssoc )
    quickCheck (semigroupAssoc :: OrAssoc )
    -- Can't get this to work, no instance of Eq for functions
    -- quickCheck (semigroupAssoc :: CombineAssoc )


