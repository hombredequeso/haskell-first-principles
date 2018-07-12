module FizzBuzz23 where

import Control.Monad
import Control.Monad.Trans.State
-- import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

-- Without state:
--
-- main :: IO ()
-- main =
--     mapM_ (putStrLn . fizzBuzz) [1..100]

-- the 'state' is a [String].
-- The result of addResult is ()
addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

-- addResult' :: Integer -> State [String] ()
-- addResult' n = 
--     let result = fizzBuzz n in
--         state (\ss -> ((), result : ss ))


addResult' :: Integer -> State [String] ()
addResult' n = state (\st -> 
    ((), fizzBuzz n : st ))

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
    execState (mapM_ addResult list) []

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = fmap fizzBuzz [to, to-1..from]

listDownTo start stop  
  | start >= stop = [start .. stop]
  | start < stop = [stop,stop -1 .. start]

fizzBuzzFromTo'' :: Integer -> Integer -> [String]
fizzBuzzFromTo'' start stop = fizzBuzz <$> listDownTo start stop

fizzbuzzFromTo' :: Integer -> Integer -> [String]
fizzbuzzFromTo' from to = fizzbuzzDownTo to (to - from)

fizzbuzzDownTo :: Integer -> Integer -> [String]
fizzbuzzDownTo to 0 = [fizzBuzz to]
fizzbuzzDownTo to offset = (fizzBuzz (to + offset)) : fizzbuzzDownTo to (offset - 1)

fizzbuzzDownTo' :: Integer -> Integer -> [String]
fizzbuzzDownTo' to offset = foldr (\a acc -> fizzBuzz a : acc) [] [to+offset, to+offset-1, to]

main :: IO ()
main =
    mapM_ putStrLn $
        reverse $ fizzbuzzList [1..100]
