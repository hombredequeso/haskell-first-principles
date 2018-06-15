module TreeEx where

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)


instance Functor Tree where
    fmap _ Empty = Empty
    fmap a2b (Leaf a) = Leaf $ a2b a
    fmap a2b (Node ta1 a ta2) = 
        Node (fmap a2b ta1) (a2b a) (fmap a2b ta2)

instance Foldable Tree where
    --  foldMap :: (a -> m) -> Tree a -> m
    --  Monoid m
    foldMap _ Empty = mempty
    foldMap a2m ( Leaf a ) = a2m a
    foldMap a2m (Node ta1 a ta2) = (foldMap a2m ta1) `mappend` (a2m a) `mappend` (foldMap a2m ta2)

instance Traversable Tree where
    -- traverse:: (a -> f b) -> Tree a -> f (Tree b)
    -- Applicative f
    traverse a2fb Empty = pure Empty
    traverse a2fb (Leaf a) = Leaf <$> (a2fb a)
    traverse a2fb (Node ta1 a ta2) = 
        Node <$> (traverse a2fb ta1) <*> (a2fb a) <*> (traverse a2fb ta2)

