import Control.Monad (join)

-- Defining bind in terms of:
--  fmap
--  join

-- Defined in GHC.base
-- fmap :: Functor f => (a -> f b) -> f a -> f (f b)

-- imported above
-- join :: Monad m => m (m a) -> m a

-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join (fmap f ma)

bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn ("y helo thar: " ++ name)

printHelloLine name = putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStrLn "name pls:" >> 
        getLine >>= 
        printHelloLine

twoBinds :: IO ()
twoBinds = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn "age pls:"
    age <- getLine
    putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")

getWelcomeLine name age = 
    ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
    putStrLn "name pls:" >>
    getLine >>=
    \name -> putStrLn "age pls:" >>
    getLine >>=
    \age -> putStrLn $ getWelcomeLine name age

