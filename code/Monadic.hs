
module FT 
    ( runFTIn
    , runFTWith
    , runFT
    , unsafeLiftFromIO )
         where

import Context
import System.IO.Unsafe

newtype FT c a = FT (Context c -> a)

instance Monad (FT c) where
    return a = FT (\_ -> a) 
    (>>=) (FT a) f = FT (\c -> (\(FT b) -> b c) $ f $ a c)


runFTIn context (FT a) = a context

runFTWith options = runFTIn (unsafePerformIO $ getContext options)

runFT = runFTWith []

unsafeLiftFromIO a = FT $ (\c -> unsafePerformIO $ a c)
