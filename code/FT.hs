
newtype FT c a = FT (Context -> a)

instance Functor (FT c) where
    fmap f (FT a) = FT (f.a)

instance Applicative (FT c) where
    pure a = FT (\_ -> a)
    (<*>) (FT a) (FT b) = FT (\c -> a c $ b c)

instance Monad (FT c) where
    return = pure 
    (>>=) (FT a) f = FT (\c -> (\(FT b) -> b c) $ f $ a c)

runFTIn :: Context -> (forall c. FT c a) -> a
runFTIn context (FT a) = a context


runFTWith :: [ContextOption] -> (forall c. FT c a) -> a
runFTWith options a 
    = unsafePerformIO 
    $ getContext options >>= \c -> return $ runFTIn c a 
runFT = runFTWith []

-- placeholders
--runFTWith = id
--runFT = id

unsafeLiftFromIO :: (Context -> IO a) -> FT c a
unsafeLiftFromIO a = FT (\c -> unsafePerformIO $ a c)
