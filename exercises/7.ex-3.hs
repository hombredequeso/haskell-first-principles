g :: (a -> b) -> (a, c) -> (b, c)
g aTob (a,c) = (aTob a, c)
