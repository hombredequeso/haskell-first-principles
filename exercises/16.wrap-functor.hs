data Wrap f a =
    Wrap (f a)
    deriving (Eq, Show)

-- Note: because there is 1 argument to the Wrap constructor function (f a),
--  it is only possible to match on (f a) in its entirety, hence, "fa" below
instance Functor f 
  => Functor (Wrap f) where
      fmap f' (Wrap fa) = Wrap (fmap f' fa)


-- Which is different to:

data Wrap' f a =
    Wrap' f a
    deriving (Eq, Show)

instance Functor (Wrap' f) where
    fmap f' (Wrap' f a) = Wrap' f (f' a)

-- because Wrap' is basically:
--
data MyTuple f g =
    MyTuple f g
    deriving (Eq, Show)

instance Functor (MyTuple f) where
    fmap f' (MyTuple f g) = MyTuple f (f' g)
