import Control.Applicative

boop = (*2)
doop = (+10)

-- bip and bloop are the same thing...

bip :: Integer -> Integer
bip = boop . doop

-- fmap a function over another function
bloop :: Integer -> Integer
bloop = fmap boop doop
-- bloop i = ((fmap boop) doop) i
-- bloop i = boop (doop i)

-- bbap and duwop are the same thing (but different from bip and bloop)

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

