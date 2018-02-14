module Arith4 where

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read b) => a -> b
roundTrip x = (b::read (show x))

main = do
    print (roundTrip 4)
    print (id 4)
