module MonadExercisesMonadicFunctions where

-- write these operations using only fmap and monadic operations
-- (i.e. can't also use applicative)
-- Of course, can derive applicative and functor from monadic operations anyway...

-- class Applicative m => Monad (m :: * -> *) where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a

--   (>>=) :: [] a -> (a -> [] b) -> [] b

-- join
-- same as flat map, where the func is id
j :: Monad m => m (m a) -> m a
-- transforms each ma into mb using id
-- j m(m a) = m(m a) >>= id
j x = x >>= id


-- lift 1, same as fmap
l1 :: Monad m => (a -> b) -> m a -> m b
-- type of (return . a2b) is a -> m b
l1 a2b ma = ma >>= (return . a2b)
-- l1 a2b ma = (return a2b) <*> ma

l1b :: Monad m => (a -> b) -> m a -> m b
l1b a2b ma = fmap a2b ma

-- lift 2.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 a2b2c ma mb = (return a2b2c) <*> ma <*> mb
-- l2 a2b2c ma mb = (a2b2c <$> ma) <*> mb

l2c :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2c a2b2c ma mb = do
                    a <- ma
                    b <- mb
                    return (a2b2c a b)

l2d :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2d a2b2c ma mb = ma >>= (\a -> 
                            mb >>= \b ->
                                return (a2b2c a b)
                         )

-- Applicative apply
a :: Monad m => m a -> m (a -> b) -> m b
a ma ma2b = ma2b >>= (\a2b -> fmap a2b ma)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (h:t) f = l2 (:) (f h)  (meh t f)
--              m b : m [b]
--              : does 
--                  b   [b]
--              we want :
--                  (m b) (m [b])
--              so lift 2 will lift : from being
--              a -> b -> c
--              to
--              m a -> m b -> m c

flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas id 
-- meh :: [a] -> (a -> m b) -> m [b]
--        [ma]  id: ma -> m a -> m [a]

liftedCons:: (Monad m) => m a -> m [a] -> m [a]
liftedCons = l2 (:)

flipType2 :: (Monad m) => [m a] -> m [a]
flipType2 [] = return []
flipType2 (ma:mas) = l2 (:) ma (flipType2 mas)

