{-# LANGUAGE InstanceSigs #-}

module StateMoi where

-- 'Moi' is just State, which is _just_ a wrapped function.
newtype Moi s a =
    Moi { runMoi :: s -> (a, s) }

-- Functor

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    -- g is (s -> (a, s))
    fmap f (Moi g) = Moi (\s -> 
        let (a,s2) = g(s) in
        (f(a), s2))


instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi (\s -> (a, s))

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = 
        Moi (\s -> 
                let
                    (f', s2) = f(s)
                    (a,s3) = g(s2) 
                in (f'(a), s3)
            )


instance Monad (Moi s) where
    return = pure
    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi sa) >>= g = Moi (\s ->
                                let 
                                    (a, s2) = sa(s)
                                    moiSb = (g(a))
                                 in runMoi moiSb s2
                         )

