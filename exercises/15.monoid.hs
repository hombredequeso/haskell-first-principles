module Ex15Monoid where

import Data.Monoid (Monoid, mempty, mappend, Sum(..), getSum)
import Data.Semigroup (Semigroup, (<>))
import Test.QuickCheck
import Text.Show.Functions


-- /////////////////////////////////////////////////////////////
-- Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- /////////////////////////////////////////////////////////////

newtype Identity a =
    Identity a 
    deriving (Eq)

instance Semigroup a => Semigroup (Identity a) where
    Identity x <> Identity y = Identity (x <> y) 

instance Show a => Show (Identity a) where
    show (Identity a) = "Identity : " ++ show a

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
    -- Treating arbitrary as a functor:
    arbitrary = Identity <$> arbitrary

    -- Which is equivalent to (monad):
    --arbitrary = 
    --    do
    --        a <- arbitrary
    --        return (Identity a)
    --
    -- monad again:
    -- arbitrary = arbitrary >>= \x -> return (Identity x)


type IdentityStringAssoc = Identity String -> Identity String -> Identity String -> Bool

-- /////////////////////////////////////////////////////////////

data Two a b = Two a b deriving Eq

instance (Show a, Show b) => Show (Two a b) where
    show (Two a b) = "Two : " ++ show a ++ "; " ++ show b

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a<>a') (b<>b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    -- arbitrary created using applicative:
    arbitrary = Two <$> arbitrary <*> arbitrary

-- instance (Monoid a, Monoid b, Semigroup a, Semigroup b) => Monoid (Two a b) where
instance (Monoid a, Monoid b, Semigroup a, Semigroup b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- /////////////////////////////////////////////////////////////

newtype BoolConj =
    BoolConj Bool
    deriving Eq

instance Show BoolConj where
    show (BoolConj b) = "BoolConj : " ++ (show b)

instance Arbitrary BoolConj where
    arbitrary = BoolConj <$> arbitrary

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    (BoolConj False) <> (BoolConj _) = BoolConj False
    (BoolConj _) <> (BoolConj False) = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)


type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- /////////////////////////////////////////////////////////////

newtype Combine a b =
    Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
    show (Combine f) = "Combine : " ++ show f

instance Semigroup b => Semigroup (Combine a b) where
    Combine f1 <> Combine f2 = Combine (\a -> f1 a <> f2 a)

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine (\_ -> mempty)
    mappend = (<>)
-- /////////////////////////////////////////////////////////////

newtype Mem s a =
    Mem {
        runMem :: s -> (a,s)
    }

instance Show (Mem s a) where
    show (Mem f) = "Mem, runMem =  " ++ show f
-- Run f1, and then f2, passing the changing state result through them.
--         Final a value is the two a results semigrouped together
instance Semigroup a => Semigroup (Mem s a) where
    Mem f1 <> Mem f2 = Mem (\startState -> 
                                let (a1 , newState) = f1 startState 
                                    (a2 , finalState) = f2 newState
                                    a = a1 <> a2
                                in (a, finalState))

instance (Monoid a, Semigroup a) => Monoid (Mem s a) where
    mempty = Mem {runMem = \s -> (mempty, s)}
    mappend = (<>)

-- What does it do? Treats the data part (a) like regular monoid/semigroup,
--  but passes state (s) through the execution of each instance of Mem.
--  For example, counting how many instances of "a" were used to
--  get the final "a" result (i.e. a counter), where all the instances
--  of a were put together in the usual <> way:
--
    --  f' = Mem $ \s -> ("hi", s + 1)
    --  
--  f' = Mem $ \s -> ("hi", s + 1)
--  f = Mem $ \s -> ("bye", s + 1)
--  runMem (f <> f') 0
--   results in:
--      ("byehi",2)
-- /////////////////////////////////////////////////////////////

semigroupAssoc :: (Eq m, Semigroup m) 
    => m 
    -> m 
    -> m 
    -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m)
    => m
    -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m)
    => m
    -> Bool
monoidRightIdentity a = (a <> mempty) == a
-- /////////////////////////////////////////////////////////////
main :: IO ()
main = do
  putStrLn "\n Trivial"
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  quickCheck (semigroupAssoc :: IdentityStringAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String-> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)

  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

