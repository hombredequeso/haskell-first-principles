{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

-- Identity:
newtype Identity a =
    Identity { runIdentity :: a }
    deriving (Eq, Show)

-- IdentityT:
-- outer wrapper is Identity, f is inner wrapper.
newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)



instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

-- given that m is a Functor, then
--  Functor (IdetntityT m) is possible.
instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT ma) = IdentityT (fmap f ma)

-- ******************************************************
-- Applicative
-- ******************************************************

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
    pure a = IdentityT (pure a)
    (IdentityT ma2b) <*> (IdentityT ma) = IdentityT (ma2b <*> ma)

-- ******************************************************
-- Monad
-- ******************************************************

instance Monad Identity where
    return = pure
    (Identity a) >>= a2mb = a2mb a

instance (Monad m) => Monad (IdentityT m) where
    return = pure
    -- (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    --    This illustrates the fundamental problem, that it is necessary to get a a2mb, when we only have a2Imb. So, it is necessary to know about I (IdentityT)
    -- (>>=)       (IdentityT ma)    a2Imb               =  ma >>= a2Imb
    --                                                      ^^^ This is the bind on monad m, of type ma >>= a2mb.
    --                                                      Problem is, we only have an a2Imb (when we want an a2mb)

    (>>=) (IdentityT ma) a2Imb = IdentityT $ ma >>= runIdentityT.a2Imb
