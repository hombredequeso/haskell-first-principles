{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

-- Identity:
newtype Identity a =
    Identity { runIdentity :: a }
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

-- Compose:
newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) =
        Compose $ (fmap . fmap) f fga

-- Exercise: write Compose Applicative
--
-- https://carlo-hamalainen.net/2014/01/02/applicatives-compose-monads-do-not/
-- And watch his live coding demo to get Applicative Compose:
--   https://www.youtube.com/watch?time_continue=748&v=AjtQ0sQaHn0

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure = Compose . pure . pure 

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (<*>) (Compose f) (Compose x) = Compose (liftA2 (<*>) f x)
    -- (Compose fgfunc) <*> (Compose fga) =  Compose 
    --                                         $ ( convert fgfunc ) <*> 
    --                                             fga where
    --                                                 convert f = undefined

ff:: (Foldable f, Foldable g, Monoid m, Functor f) => (a -> m) -> f  ( g a )  -> f m
ff a2m fga = fmap ( foldMap a2m ) fga

doubleFold :: (Foldable t2, Foldable t1, Monoid m) => (a -> m) -> t1 (t2 a) -> m
doubleFold = foldMap. foldMap

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap a2m (Compose fga) = foldMap (foldMap a2m) fga
    -- foldMap a2m (Compose fga) = doubleFold a2m fga
    
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse a2fb (Compose ca) = 
        fmap Compose $ traverse (traverse a2fb) ca

-- One less piece of structure than Compose (or one more than Identity)

newtype One f a =
    One (f a)
    deriving (Eq, Show)

instance Functor f' => Functor (One f') where
        fmap f (One fa) = One $ fmap f fa

-- One more piece of structure than Compose

newtype Three f g h a =
    Three (f (g (h a)))
    deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
    fmap f (Three fgha) =
        Three $ (fmap . fmap . fmap) f fgha

-- Messin'
--

-- a :: Compose [] Maybe
-- a = Compose [Just 1]
-- Composing composes

v :: Compose [] Maybe (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]

vv :: Compose [] Maybe (Compose Maybe [] a)
vv = Compose [Just (Compose $ Just [])]

main:: IO()
main =  do
    print "abc"

    let i = Identity 123
    let (Identity ii) = i
    print ii
    let z = runIdentity i
    print z


    let one' = One (Just 1)
    let two = ( (+)1 ) <$> one'
    print two

