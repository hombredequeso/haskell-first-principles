-- 1. -- Type
-- []
-- -- Methods
-- pure :: a -> ? a
-- (<*>) :: ? (a -> b) -> ? a -> ? b
--
    
pure :: a -> [] a
(<*>) :: [] (a -> b) -> [] a -> [] b
--
-- 2. -- Type
-- IO
-- -- Methods
-- pure :: a -> ? a
-- (<*>) :: ? (a -> b) -> ? a -> ? b
pure :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b
-- 3. -- Type
-- (,) a
-- -- Methods
pure :: a -> (a,) 
(<*>) :: (a,) (a -> b) -> (a,) -> (b,)

-- 4. -- Type
-- (->) e
-- -- Methods
pure :: a -> ? a
(<*>) :: ? (a -> b) -> ? a -> ? b

-- Maybe take a look at:
--https://www.reddit.com/r/haskell/comments/1olk04/the_magic_of_join/
-- https://en.wikibooks.org/wiki/Haskell/Understanding_arrows
    --note that:
    -- We can write (e -> a) as (->) e a
