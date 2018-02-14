{-# LANGUAGE FlexibleInstances #-}

module Chapter16Exercises where


import GHC.Arr

data Bool =
    False | True
-- nope, no functor. Can see that because there is no way to get a * -> * kind out of Bool.


data BoolAndSomethingElse a =
    False' a | True' a
    deriving Show

-- :kind BoolAndSomethingElse = * -> *
-- therefore:

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' (f a)
    fmap f ( True' a ) = True' (f a)

--  fmap (\x-> x + 1)  (False' 4)
    -- Because:
    -- :type ($)
    -- ($) :: (a -> b) -> a -> b
--  fmap (\x-> x + 1) $  False' 4
--  (   a -> b )      $      a      -> b
--  (\x -> x + 1) <$> (False' 4)
--  (\x -> x + 1) <$> False' 4

data BoolAndMaybeSomethingElse a =
    Falsish | Truish a
    deriving (Show, Eq)

-- Pretty much same as previous one, just one of the sum types doesn't happen to have a.

instance Functor  BoolAndMaybeSomethingElse where
    fmap f Falsish = Falsish
    fmap f (Truish a) = Truish (f a)

-- *Chapter16Exercises> add1 = (+) 1
-- *Chapter16Exercises> :t add1
-- add1 :: Num a => a -> a
-- *Chapter16Exercises> fmap add1 (Truish 3)
-- Truish 4
-- *Chapter16Exercises> fmap add1 Falsish
-- Falsish
-- *Chapter16Exercises>


-- A little diversion on the way to figuring out Mu
-- Changed f (from what is written in the book) to 't' to avoid
-- subsequent confusion, where in the Functor for Wrap where 'f'
-- is used in two different scopes.

data Wrap t a =
    Wrap (t a)
    deriving (Eq, Show)

-- Imagine t is itself a functor: List, Maybe, ...
--
-- x = Wrap (Just 1)
-- l = Wrap ([1,2,3])

-- so we've wrapped something that is a functor, which 
-- means whatever gets unwrapped (f a) can have fmap called on it.


instance Functor t
    => Functor (Wrap t) where
        fmap f (Wrap fa) = Wrap (fmap f fa)
        -- so, call fmap on a Wrap value, unwrap it, and can fmap on the unwrapped value.


-- ??????????????//
--
-- *Chapter16Exercises> :t Wrap
-- Wrap :: f a -> Wrap f a
-- *Chapter16Exercises> :k Wrap
-- Wrap :: (* -> *) -> * -> *
-- *Chapter16Exercises> :k Mu
-- Mu :: (* -> *) -> *
-- *Chapter16Exercises>
-- *Chapter16Exercises>

newtype Mu f = InF { outF :: f (Mu f) }

-- the "y combinator"
-- Looks like technically, can't be done. Although with qualifications something similar to fmap can be achieved.
--     https://stackoverflow.com/questions/39770191/functor-instance-for-newtype-mu-f-inf-outf-f-mu-f
--
-- See too:
--          http://www.ece.uc.edu/~franco/C511/html/Scheme/ycomb.html
--          https://stackoverflow.com/questions/4273413/y-combinator-in-haskell:

-- Note that by comparison with Wrap, it can seen why it can't be done:
-- *Chapter16Exercises> :k Wrap
-- Wrap :: (* -> *) -> * -> *
-- so if the function (*->*) is consumed, * -> * remains, so it is possible to have a functor.
-- However:
-- *Chapter16Exercises> :k Mu
-- Mu :: (* -> *) -> *
-- No way to get to the kind:  * -> *

data D a =
    D Int Int (a Word Word)

-- Similar to Mu, no instance functor possible. (can't get kind of *->* out of this)
-- *Chapter16Exercises> :k D
-- D :: (* -> * -> *) -> *
-- *Chapter16Exercises>

-- Just switch the "a b" to "b a"
-- because the Functor wants to operate on the First value
    
data Sum b a =
    First a
    | Second b

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b

data Company a c b =
    DeepBlue a c
    | Something b

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c


data More b a =
    L a b a
    | R b a b
    deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

data Quant a b =
    Finance
    | Desk a
    | Bloor b

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

-- data K a b =
--     K a
-- 
-- instance Functor (K a) where
--     fmap f (K a) = K a

newtype K a b =
    K' a
    deriving (Show, Eq)

instance Functor (K a)  where
    fmap f (K' a) = K' a

newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)


instance Functor (Flip K a) where
    fmap f (Flip (K' b)) = Flip (K' (f b))

-- *Chapter16Exercises> a = Flip (K' 123)
-- *Chapter16Exercises> fmap (\x -> x + 1) a
-- Flip (K' 124)
-- *Chapter16Exercises>

data EvilGoateeConst a b =
    GoatyConst b
    deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a =
    LiftItOut (f a)

instance Functor f =>
    Functor (LiftItOut f) where
        fmap f' (LiftItOut fa) = LiftItOut (fmap f' fa)

data Parappa f g a =
    DaWrappa (f a) (g a)

instance (Functor f, Functor g) =>
    Functor (Parappa f g) where
        fmap f' (DaWrappa fa ga) = DaWrappa (fmap f' fa) (fmap f' ga)

data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)

instance (Functor g) =>
    Functor (IgnoreOne f g a) where
        fmap f' (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f' gb)

data Notorious g o a t =
    Notorious (g o) (g a) (g t)
    deriving (Show, Eq)

instance (Functor g) =>
    Functor (Notorious g o a) where
        fmap f' (Notorious go ga gt) = Notorious go ga (fmap f' gt)

-- *Chapter16Exercises> n = Notorious (Just 'c') (Just "abc") (Just 123)
-- *Chapter16Exercises> fmap (\x -> x + 1) n
-- Notorious (Just 'c') (Just "abc") (Just 124)

data List a =
    Nil
    | Cons a (List a)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons h t) = Cons (f h) (fmap f t)

data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a)
                (GoatLord a)
                (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats g1 g2 g3) =
        MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap f Halt = Halt
    fmap f (Print str a) = (Print str (f a))
    fmap f (Read s2a) = Read (f.s2a)

