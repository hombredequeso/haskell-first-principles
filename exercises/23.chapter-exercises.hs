{-# LANGUAGE InstanceSigs #-}

module ChapterExercises23 where


-- 'State' is just State, which is _just_ a wrapped function.
-- 'State' is copied from 23.state-moi.hs, with Moi renamed to State

newtype State s a =
    State { runState :: s -> (a, s) }

-- Functor

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    -- g is (s -> (a, s))
    fmap f (State g) = State (\s -> 
        let (a,s2) = g(s) in
        (f(a), s2))


instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State (\s -> (a, s))

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (State f) <*> (State g) = 
        State (\s -> 
                let
                    (f', s2) = f(s)
                    (a,s3) = g(s2) 
                in (f'(a), s3)
            )


instance Monad (State s) where
    return = pure
    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State sa) >>= g = State (\s ->
                                let 
                                    (a, s2) = sa(s)
                                    moiSb = (g(a))
                                 in runState moiSb s2
                         )


get :: State s s
get = State (\s -> (s,s))


put :: s -> State s ()
put s = State (\x -> ((), s))

exec :: State s a -> s -> s
exec (State sa) s = snd (sa s)

eval :: State s a -> s -> a
eval (State sa) s = fst (sa s)

modify :: (s -> s) -> State s ()
modify s2s = State (\s -> ((), s2s s))

