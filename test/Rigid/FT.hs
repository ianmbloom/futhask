{-# LANGUAGE RankNTypes #-}
module Rigid.FT (runFTIn, runFTWith, runFT, unsafeLiftFromIO) where
import Rigid.Context
import System.IO.Unsafe

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

runFTWith options a = unsafePerformIO $ (flip runFTIn) a <$> getContext options 
runFT = runFTWith []

unsafeLiftFromIO :: (Context -> IO a) -> FT c a
unsafeLiftFromIO a = FT (\c -> unsafePerformIO $ a c)
