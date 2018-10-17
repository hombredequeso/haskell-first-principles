module Arith4 where

-- id :: a -> a
-- id x = x

roundTripA :: (Show a, Read a) => a -> a
roundTripA = read.show

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read.show

main = do
    let x = 4::Int
    print (roundTripA 4)
    let z = (roundTrip x)::Int
    print z
    print (id 4)
