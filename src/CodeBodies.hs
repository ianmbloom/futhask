{-# LANGUAGE QuasiQuotes #-}


module CodeBodies where
import Text.RawString.QQ
import Backends

typeClassesBody = [r|
class FutharkObject wrapped raw | wrapped -> raw, raw -> wrapped where
    wrapFO :: ForeignPtr raw -> wrapped c
    freeFO :: Ptr Raw.Futhark_context -> Ptr raw -> IO Int
    withFO :: wrapped c -> (Ptr raw -> IO b) -> IO b
    


class (FutharkObject array rawArray, Storable element, M.Index dim) 
    => FutharkArray array rawArray dim element 
    | array -> dim, array -> element 
    where
        shapeFA  :: Ptr Raw.Futhark_context -> Ptr rawArray -> IO (M.Sz dim)
        newFA    :: Ptr Raw.Futhark_context -> Ptr element -> M.Sz dim -> IO (Ptr rawArray)
        valuesFA :: Ptr Raw.Futhark_context -> Ptr rawArray -> Ptr element -> IO Int 

class Input fo ho where
    toFuthark :: ho -> FT c fo 

class Output fo ho where
    fromFuthark :: fo -> FT c ho
|]

configBody OpenCL = [r|
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
    (BuildOptions os)    -> mapM_ (\o -> withCString o $ Raw.context_config_add_build_option config) os
    (Debug flag)         -> Raw.context_config_set_debugging config flag
    (Profile flag)       -> Raw.context_config_set_profiling config flag
    (Log flag)           -> Raw.context_config_set_logging   config flag
    (Device s)           -> withCString s $ Raw.context_config_set_device   config
    (Platform s)         -> withCString s $ Raw.context_config_set_platform config
    (LoadProgram s)      -> withCString s $ Raw.context_config_load_program_from config 
    (LoadBinary s)       -> withCString s $ Raw.context_config_load_binary_from  config 
    (DumpBinary s)       -> withCString s $ Raw.context_config_dump_binary_to    config 
    (DefaultGroupSize s) -> Raw.context_config_set_default_group_size config s
    (DefaultGroupNum n)  -> Raw.context_config_set_default_num_groups config n
    (DefaultTileSize s)  -> Raw.context_config_set_default_tile_size  config s
    (DefaultThreshold n) -> Raw.context_config_set_default_threshold  config n
    (Size name s)        -> withCString name $ \n -> Raw.context_config_set_size config n s
                                           >>= \code -> if code == 0
                                                           then return ()
                                                           else error "invalid size"
|]


contextBody = [r|
data Context = Context (ForeignPtr Raw.Futhark_context)

getContext :: [ContextOption] -> IO Context
getContext options = do
     config <- Raw.context_config_new
     mapM_ (setOption config) options
     context <- Raw.context_new config
     Raw.context_config_free config
     fmap Context $ FC.newForeignPtr context (Raw.context_free context)

inContext (Context fp) = withForeignPtr fp
inContextWithError :: Context -> (Ptr Raw.Futhark_context -> IO Int) -> IO ()
inContextWithError context f 
    = inContext context f >>= \code 
    -> if code == 0 
            then inContext context Raw.context_sync >> return ()  
            else inContext context Raw.context_get_error 
             >>= \cs -> peekCString cs >>= \s -> F.free cs >> error s
|]


fTBody = [r|
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

unsafeLiftFromIO :: (Context -> IO a) -> FT c a
unsafeLiftFromIO a = FT (\c -> unsafePerformIO $ a c)
|]


utilsBody = [r|
wrapIn context rawObject 
    = fmap wrapFO
    $ FC.newForeignPtr
        rawObject 
        (inContextWithError context $ \c -> freeFO c rawObject)

peekFree p = peek p >>= \v -> free p >> return v
peekFreeWrapIn context rawP 
    = peek rawP >>= wrapIn context >>= \fo -> F.free rawP >> return fo


instance Input CBool Bool where 
    toFuthark b = return $ if b then fromInteger 1 else fromInteger 0

instance Output CBool Bool where
    fromFuthark cb = return $ if cb /= fromInteger 0 then True else False


-- Ptr - Dim conversion
peekAndFreeArray n a = peekArray n a >>= \a' -> free a >> return a'

to1d f cP aP
       = f cP aP
       >>= fmap (\[d0] -> M.Sz1 d0)
       . fmap (fmap fromIntegral)
       . peekAndFreeArray 1

to2d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1] -> M.Sz2 d0 d1)
       . fmap (fmap fromIntegral)
       . peekAndFreeArray 2

to3d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1, d2] -> M.Sz3 d0 d1 d2)
       . fmap (fmap fromIntegral)
       . peekAndFreeArray 3

to4d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1, d2, d3] -> M.Sz4 d0 d1 d2 d3)
       . fmap (fmap fromIntegral)
       . peekAndFreeArray 4

to5d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1, d2, d3, d4] -> M.Sz5 d0 d1 d2 d3 d4)
       . fmap (fmap fromIntegral)
       . peekAndFreeArray 5

from1d f cP eP (M.Sz1 d0)             = f cP eP (fromIntegral d0)

from2d f cP eP (M.Sz2 d0 d1)          = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)

from3d f cP eP (M.Sz3 d0 d1 d2)       = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)
                                                (fromIntegral d2)

from4d f cP eP (M.Sz4 d0 d1 d2 d3)    = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)
                                                (fromIntegral d2)
                                                (fromIntegral d3)

from5d f cP eP (M.Sz5 d0 d1 d2 d3 d4) = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)
                                                (fromIntegral d2)
                                                (fromIntegral d3)
                                                (fromIntegral d4)

instance (FutharkArray array rawArray dim element)
  => Input (array c) (M.Array M.S dim element) where
    toFuthark array = unsafeLiftFromIO $ \context
      -> inContext context $ \c
      -> MU.unsafeWithPtr array (\aP -> newFA c aP $ M.size array)
      >>= wrapIn context

instance (FutharkArray array rawArray dim element)
  => Output (array c) (M.Array M.S dim element) where
    fromFuthark array = unsafeLiftFromIO $ \context
      -> inContext context $ \c
      -> withFO array $ \aP
      -> do
          shape <- shapeFA c aP
          pointer <- mallocForeignPtrArray $ M.totalElem shape
          withForeignPtr pointer $ valuesFA c aP
          return $ M.resize' shape
                 $ MU.unsafeArrayFromForeignPtr0 M.Par pointer
                 $ M.Sz1 (M.totalElem shape)

|]

