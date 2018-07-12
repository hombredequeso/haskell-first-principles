{-# LANGUAGE InstanceSigs #-}

-- The "Reader" constructor is simply a newtype wrapper around a function, (r -> a)
-- the function is given a convenience name, runReader
newtype Reader r a =
    Reader { runReader :: r -> a }


instance Functor (Reader r) where
    fmap a2b (Reader ra) = 
        Reader $ a2b . ra

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ (\x -> a)

    (<*>) :: Reader r (a -> b)
          -> Reader r a
          -> Reader r b
    (Reader rab) <*> (Reader ra) =
        Reader $ \r -> (rab r) (ra r)

instance Monad (Reader r) where
    return = pure
    (>>=) :: Reader r a
          -> (a -> Reader r b)
          -> Reader r b
    (Reader ra) >>= aRb =
        Reader $ \r -> runReader ((aRb.ra) r ) r -- runReader (aRb ( ra r )) r


