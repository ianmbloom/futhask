{-# LANGUAGE FlexibleInstances #-}

module Debug
  ( assert
  , assertError
  , tr
  , trM
  , trWith
  , trNlWith
  , tc
  , tcWith
  , tcDepth
  , trCList
  , trDepth
  , trLength
  , trWhen
  , trWhenWith
  , tcWhen
  , trIfTrue
  , trIfFalse
  , trace
  , traceWhen
  , DebugIdentity (..)
  , debugHead
  , debugFromJust
  )
where

import Data.Word
import Data.Int
import Debug.Trace
import qualified Data.ByteString as B
import Control.Applicative

-- import Manipipe.Util.ShowNumeric

newtype DebugIdentity a = DebugIdentity { runDebugIdentity :: a }

instance Show a => Show (DebugIdentity a) where
    show = show

instance Functor DebugIdentity where
    fmap f m = DebugIdentity (f (runDebugIdentity m))

instance Applicative DebugIdentity where
    pure a = DebugIdentity a
    m <*> k = DebugIdentity $ (runDebugIdentity m) (runDebugIdentity k)

instance Monad DebugIdentity where
    return = trace "return" . pure
    m >>= k  = trace ">>=" $ k (runDebugIdentity m)

assert      message cond x = if cond then x else trace ("ASSERT "      ++ message ++ show x) x
assertError message cond x = if cond then x else error ("ASSERTERROR " ++ message) x

trWith f m x = trace (m++" "++f x) x

trNlWith f m x = trace (m++"\n"++f x) x

tr :: (Show a) => String -> a -> a
tr = trWith show
trM m = fmap (tr m)

tcWith :: (a -> String) -> String -> a -> a
tcWith f m x = trace ("START "++m) $
               trace ("END "++m++"--->"++(f x)) x

traceWhen :: Bool -> String -> a -> a
traceWhen cond message = if cond then trace message else id

tc :: Show a => String -> a -> a
tc = tcWith show

indenter depth = concat (replicate depth "   ")

tcWithDepth :: Int -> (a -> String) -> String -> a -> a
tcWithDepth i f m x = trace (indenter i ++"START "++m) $
                      trace (indenter i ++ "END "++m++"--->"++(f x)) x

tcDepth depth message = tcWithDepth depth show message

trDepth depth message = tr (indenter depth ++ message)

trCList m x = trace (m++":"++concat (zipWith (\i l ->", " ++ show i ++ ":" ++ show l) [0..] x)) x

trLength :: Show a => String -> [a] -> [a]
trLength = trWith (\x -> show (length x) ++ ":" ++ show x)

trWhen  cond m x = if cond then tr  m x else x
tcWhen  cond m x = if cond then tc  m x else x

trWhenWith f m x = if f x then tr m x else x

trIfTrue m x = if x then tr m x else x
trIfFalse m x = if not x then tr m x else x


debugHead m [] = error $ "debugHead" ++ m
debugHead m (x:_) = x

debugFromJust m Nothing = error $ "debugFromJust" ++ m
debugFromJust m (Just x) = x
