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


getContext :: forall c. [ContextOption] -> IO (Context c)
getContext options = do
     config <- Raw.context_config_new
     mapM_ (setOption config) options
     context <- Raw.context_new config
     Raw.context_config_free config
     fmap Context $ FC.newForeignPtr (Raw.context_free context) context

inContext (Context fp) = withForeignPtr fp
inContextWithError :: Context c -> (Ptr Raw.Context -> IO Int) -> IO ()
inContextWithError context f 
    = inContext context f >>= \code 
    -> if code == 0 
            then inContext context Raw.context_sync >> return ()  
            else inContext context Raw.context_get_error 
             >>= \cs -> peekCString cs >>= \s -> F.free cs >> error s


{--
(<:) :: (Input fo ho) => (fo -> a) -> ho -> a
(<:) f ho = f (toFuthark ho)

lift1 f a = fromFuthark $ f <: a
lift2 f a b = fromFuthark $ f <: a <: b
lift3 f a b c = fromFuthark $ f <: a <: b <: c
lift4 f a b c d = fromFuthark $ f <: a <: b <: c <: d
lift5 f a b c d e = fromFuthark $ f <: a <: b <: c <: d <: e
--}
