
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aTob a b =  areEqual where
                    aAsb = aTob a
                    areEqual = aAsb == b
        
arith :: Num b
            => (a -> b)
            -> Integer
            -> a
            -> b
arith aTob i a =  ans where
                    ab = aTob a
                    ans = ab

