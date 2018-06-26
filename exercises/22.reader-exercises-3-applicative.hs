{-# LANGUAGE InstanceSigs #-}



myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 a2b2c fa fb = a2b2c <$> fa <*> fb

-- because:
-- a2b2c <$> fa :: f b2c
-- f b2c <*> fb :: f c
--
    -- so what is the f?
    -- it will be a function taking some sort of argument:
    -- (->) SomeType
    --
        

-- For example:
-- the f (applicative context) here will be a function taking an integer
-- (->) Int
-- or:
-- Int ->
a2b2c :: String -> String -> String
a2b2c = ( ++ )
-- a2b2c a b = a ++ b

intPlus1ToStr :: Int -> String
intPlus1ToStr i = (show (i + 1)) ++ " "

intPlus2ToStr :: Int -> String
intPlus2ToStr i = (show (i + 2)) ++ " "

-- We started with a function that took two strings,
-- but lifted it into a context where it takes one String!
lifteda2b2c :: Int -> String
lifteda2b2c = myLiftA2 a2b2c intPlus1ToStr intPlus2ToStr


-- =========================================================================

-- The "Reader" constructor is simply a newtype wrapper around a function, (r -> a)
-- the function is given a convenience name, runReader
newtype Reader r a =
    Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
    fmap a2b (Reader ra) = 
        Reader $ a2b . ra
        -- Reader $ (\r -> a2b (ra r))

-- 3. Implement the Applicative for Reader.

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ (\x -> a)

    (<*>) :: Reader r (a -> b)
          -> Reader r a
          -> Reader r b
    (Reader rab) <*> (Reader ra) =
        -- _todo :: r -> b
        -- Reader _todo
        
        Reader $ \r -> (rab r) (ra r)

