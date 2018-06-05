-- Thanks for the great example, Alex
data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

-- There's a decoder function that makes
-- some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- There's a query, that runs against the
-- DB and returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- an additional "context initializer",
-- that also has IO
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined
    
        
-- before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do -- do an IO:
    -- a :: [String]
    a <- fetchFn query

    -- (map decodeFn a) :: [Either Err SomeObj]
    --
    -- Prelude> :t sequence
    -- sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)
    --                                         List ((Either Err) SomeObj) -> Either Err (List SomeObj)
    --                                         t = List, m = Either Err, a = SomeObj
        -- sequence (map decodeFn a) :: Either Err (List SomeObj)
        -- Suspicious: sequence + map within the context of a manual case
    case sequence (map decodeFn a) of
      -- err :: Err
      (Left err) -> 
          -- return lifts to IO.
          -- IO(Left Err)
          return $ Left $ err
      -- res:: List SomeObj
      (Right res) -> do -- do(ing) another IO. Suspicious: a do, because it is necessary to rebind.
          -- a :: [(SomeObj, IOOnlyObj)]
          a <- makeIoOnlyObj res
          -- IO Right [(SomeObj, IOOnlyObj)]
          return $ Right a

pipelineFn2 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn2 query = do
    -- a :: [String]
    a <- fetchFn query

    -- Prelude> :i Traversable
    -- class (Functor t, Foldable t) => Traversable (t :: * -> *) where
    --   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    --   sequenceA :: Applicative f => t (f a) -> f (t a)
    --   mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    --   sequence :: Monad m => t (m a) -> m (t a)

                                 -- String -> Either Err SomeObj                    [String] 
                           -- Either Err [SomeObj]
            -- [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
    traverse makeIoOnlyObj (mapM decodeFn                                           a)

