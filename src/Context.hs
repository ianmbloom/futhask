{-# LANGUAGE RankNTypes, ExistenstialQuantification #-}

module Context where

import qualified Foreign.Concurrent as FC
import Foreign as F

data Context c = Context (ForeignPtr Raw.Context)

data ContextOption
    = BuildOptions [String]
    | Debug Int
    | Profile Int
    | Log Int
    | Device String
    | Platform String
    | LoadProgram String
    | LoadBinary String
    | DumpBinary String
    | DefaultGroupSize Int
    | DefaultGroupNum Int
    | DefaultTileSize Int
    | DefaultThreshold Int
    | Size String CSize

setOption config option = case option of
    (BuildOptions os)    -> mapM_ (\o -> withCString o $ Raw_context.config_set_debugging config) os
    (Debug flag)         -> Raw.context_config_set_debugging config flag
    (Profile flag)       -> Raw.context_config_set_profiling config flag
    (Log flag)           -> Raw.context_config_set_logging   config flag
    (Device s)           -> withCString s $ Raw.context_config_set_device   config
    (Platform s)         -> withCString s $ Raw.context_config_set_platform config
    (LoadProgram s)      -> withCString s $ Raw.context_config_set_load_progam_from config 
    (LoadBinary s)       -> withCString s $ Raw.context_config_set_load_binary_from config 
    (DumpBinary s)       -> withCString s $ Raw.context_config_set_dump_binary_to   config 
    (DefaultGroupSize s) -> Raw.context_config_set_default_group_size config s
    (DefaultGroupNum n)  -> Raw.context_config_set_default_group_num  config n
    (DefaultTileSize s)  -> Raw.context_config_set_default_tile_size  config s
    (DefaultThreshold n) -> Raw.context_config_set_default_threshold  config n
    (Size name s)        -> withCString name $ \n -> Raw.context_config_set_size config n s
                                           >>= \code -> if code == 0
                                                           then return ()
                                                           else error "invalid size"


getContext :: forall c. [ContextOption] -> IO Context c
getContext options = do
     config <- Raw.context_config_new
     mapM_ (setOption config) options
     context <- Raw.context_new config
     Raw.context_config_free config
     fMap Context $ FC.newForeignPtr (Raw.context_free context) context

inContext (Context fp) = withForeignPtr fp
inContextWithError :: Context -> (Ptr Raw.Context -> IO Int) -> IO ()
inContextWithError context f 
    = inContext context f >>= \code 
    -> if code == 0 
            then return ()
            else inContext context Raw.context_get_error 
             >>= \cs -> peekCString cs >>= \s -> F.free cs >> error s


class FutharkObject wrapped raw | wrapped -> raw where
    wrapper :: ForeignPtr raw -> wrapped
    freeFO :: Ptr Futhark_context -> Ptr raw -> IO Int
    withFO :: wrapped -> (Ptr raw -> IO b) -> IO b
    

wrapIn context rawObject 
    = fmap wrapper 
    $ FC.newForeignPtr 
        (inContextWithError context $ \c -> freeFO c rawObject)
        rawObject

peekAndWrapIn context rawP 
    = peek rawP >>= wrapIn context >>= \fo -> F.free rawP >> return fo


class (FutharkObject array, Storable element, Index dim) 
    => FutharkArray array dim element 
    | array -> dim , array -> element 
    where
        shape
           ::
        new
        values
{--    
class (FutharkObject fo) 
    => Dual fo ho
    | fo -> ho 
    where
        toFuthark   :: ho -> fo
        fromFuthark :: fo -> ho
--}


class Input fo ho where
    toFuthark :: Context -> ho -> fo

class Output fo ho where
    fromFuthark :: fo -> ho


{--
instance (Functor f, Output a) => Output (f a) where
    fromFuthark = fmap fromFuthark
--}

instance (Output a, Output b) => Output (a, b) where
    fromFuthark (a, b) = (fromFuthark a, fromFuthark b)

instance (Output a, Output b, Output c) => Output (a, b, c) where
    fromFuthark (a, b, c) = (fromFuthark a, fromFuthark b)

instance (Output a, Output b, Output c, Output d) => Output (a, b, c, d) where
    fromFuthark (a, b, c, d) = (fromFuthark a, fromFuthark b, fromFuthark c, fromFuthark d)

instance (Output a, Output b, Output c, Output d, Output e) => Output (a, b, c, d, e) where
    fromFuthark (a, b, c, d, e) = (fromFuthark a, fromFuthark b, fromFuthark c, fromFuthark d, fromFuthark e)

class Native a
instance Native Float
instance Native Double 
instance Native Int8
instance Native Int16
instance Native Int32
instance Native Int64
instance Native Word8
instance Native Word16
instance Native Word32
instance Native Word64

instance (Native a) => Input a a
    where
        toFuthark _ = id

instance (Native a) => Output a a
        fromFuthark = id

instance Dual CBool Bool
    where
        toFuthark b = if b then CBool 1 else CBool 0
        fromFuthark b = if b > 0 then True else False        

instance Dual (FutharkArray array dim element)
    where  

(<:) :: (Input fo ho) => (fo -> a) -> ho -> a
(<:) f ho = f (toFuthark ho)

lift1 f a = fromFuthark $ f <: a
lift2 f a b = fromFuthark $ f <: a <: b
lift3 f a b c = fromFuthark $ f <: a <: b <: c
lift4 f a b c d = fromFuthark $ f <: a <: b <: c <: d
lift5 f a b c d e = fromFuthark $ f <: a <: b <: c <: d <: e
