module BifunctorEx where

import Prelude hiding (Either, Left, Right)

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b)
          -> (c -> d)
          -> p a c
          -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id


data Deux a b = Deux a b

instance Bifunctor Deux where
    bimap a2b c2d (Deux a c) = Deux (a2b a) (c2d c)

data Const a b = Const a

instance Bifunctor Const where
    bimap a2b c2d (Const a) = Const (a2b a) 


data Drei a b c = Drei a b c

instance Bifunctor ( Drei x ) where
    bimap a2b c2d (Drei x a c) = Drei x (a2b a) (c2d c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
    bimap f1 f2 (SuperDrei a b) = SuperDrei a (f1 b)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
    bimap f g (SemiDrei a ) = SemiDrei a 

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either a b =
    Left a
    | Right b

instance Bifunctor (Either) where
    bimap f g (Left a) = Left (f a)
    bimap f g (Right b) = Right (g b)


