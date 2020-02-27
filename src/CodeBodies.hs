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
    toFuthark :: ho -> FT c (fo c)

class Output fo ho where
    fromFuthark :: fo c -> FT c ho

|]

configBody C = [r|
data ContextOption
    = Debug Int
    | Log Int

setOption config option = case option of
    (Debug flag)         -> Raw.context_config_set_debugging config flag
    (Log flag)           -> Raw.context_config_set_logging   config flag
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

configBody Cuda = [r|
data ContextOption
    = NvrtcOptions [String]
    | Debug Int
    | Log Int
    | Device String
    | LoadProgram String
    | DumpProgram String
    | LoadPtx String
    | DumpPtx String
    | DefaultGroupSize Int
    | DefaultGroupNum Int
    | DefaultTileSize Int
    | DefaultThreshold Int
    | Size String CSize

setOption config option = case option of
    (NvrtcOptions os)    -> mapM_ (\o -> withCString o $ Raw.context_config_add_nvrtc_option config) os
    (Debug flag)         -> Raw.context_config_set_debugging config flag
    (Log flag)           -> Raw.context_config_set_logging   config flag
    (Device s)           -> withCString s $ Raw.context_config_set_device   config
    (LoadProgram s)      -> withCString s $ Raw.context_config_load_program_from config 
    (DumpProgram s)      -> withCString s $ Raw.context_config_dump_program_to   config 
    (LoadPtx s)          -> withCString s $ Raw.context_config_load_ptx_from     config 
    (DumpPtx s)          -> withCString s $ Raw.context_config_dump_ptx_to       config 
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
data Context = Context (MVar Int) (ForeignPtr Raw.Futhark_context)

getContext :: [ContextOption] -> IO Context
getContext options = do
     config <- Raw.context_config_new
     mapM_ (setOption config) options
     context <- Raw.context_new config
     Raw.context_config_free config
     childCount <- S.newMVar 0
     fmap (Context childCount)
        $ FC.newForeignPtr context 
        $ (forkIO $ freeContext childCount context)
        >> return ()

freeContext childCount pointer 
    = readMVar childCount >>= \n 
    -> if n == 0 
        then Raw.context_free pointer 
        else yield >> freeContext childCount pointer

inContext (Context _ fp) = withForeignPtr fp
inContextWithError :: Context -> (Ptr Raw.Futhark_context -> IO Int) -> IO ()
inContextWithError context f 
    = inContext context f >>= \code 
    -> if code == 0 
            then inContext context Raw.context_sync 
--              >> inContext context Raw.context_clear_caches --OpenCL specific
              >> return ()  
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


wrapBody = [r|
wrapIn context@(Context childCount pointer) rawObject 
    =  S.modifyMVar_ childCount (return.(+1))
    >> (fmap wrapFO $ FC.newForeignPtr rawObject freeCall)
    where freeCall = (inContextWithError context $ \c -> freeFO c rawObject)
                  >> S.modifyMVar_ childCount (return.(+(-1)))

peekFree p = peek p >>= \v -> free p >> return v
peekFreeWrapIn context rawP 
    = peek rawP >>= wrapIn context >>= \fo -> F.free rawP >> return fo

-- Ptr - Dim conversion

to1d f cP aP
       = f cP aP
       >>= fmap (\[d0] -> M.Sz1 d0)
       . fmap (fmap fromIntegral)
       . peekArray 1

to2d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1] -> M.Sz2 d0 d1)
       . fmap (fmap fromIntegral)
       . peekArray 2

to3d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1, d2] -> M.Sz3 d0 d1 d2)
       . fmap (fmap fromIntegral)
       . peekArray 3

to4d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1, d2, d3] -> M.Sz4 d0 d1 d2 d3)
       . fmap (fmap fromIntegral)
       . peekArray 4

to5d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1, d2, d3, d4] -> M.Sz5 d0 d1 d2 d3 d4)
       . fmap (fmap fromIntegral)
       . peekArray 5


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

|]

utilsBody = [r|

instance (FutharkArray array rawArray dim element)
  => Input array (M.Array M.S dim element) where
    toFuthark array = unsafeLiftFromIO $ \context
      -> inContext context $ \c
      -> MU.unsafeWithPtr array (\aP -> newFA c aP $ M.size array)
      >>= wrapIn context

instance (FutharkArray array rawArray dim element)
  => Output array (M.Array M.S dim element) where
    fromFuthark array = unsafeLiftFromIO $ \context
      -> inContext context $ \c
      -> withFO array $ \aP
      -> do
          shape <- shapeFA c aP
          pointer <- mallocForeignPtrArray $ M.totalElem shape
          withForeignPtr pointer $ valuesFA c aP
          return $ M.resize' shape
                 $ MU.unsafeArrayFromForeignPtr0 M.Seq pointer
                 $ M.Sz1 (M.totalElem shape)

fromFutharkT2 (a, b) = do
    a' <- fromFuthark a
    b' <- fromFuthark b
    return (a', b')

fromFutharkT3 (a, b, c) = do
    a' <- fromFuthark a
    b' <- fromFuthark b
    c' <- fromFuthark c
    return (a', b', c')

fromFutharkT4 (a, b, c, d) = do
    a' <- fromFuthark a
    b' <- fromFuthark b
    c' <- fromFuthark c
    d' <- fromFuthark d
    return (a', b', c', d')

fromFutharkT5 (a, b, c, d, e) = do
    a' <- fromFuthark a
    b' <- fromFuthark b
    c' <- fromFuthark c
    d' <- fromFuthark d
    e' <- fromFuthark e
    return (a', b', c', d', e')

fromFutharkT6 (a, b, c, d, e, f) = do
    a' <- fromFuthark a
    b' <- fromFuthark b
    c' <- fromFuthark c
    d' <- fromFuthark d
    e' <- fromFuthark e
    f' <- fromFuthark f
    return (a', b', c', d', e', f')

fromFutharkT7 (a, b, c, d, e, f, g) = do
    a' <- fromFuthark a
    b' <- fromFuthark b
    c' <- fromFuthark c
    d' <- fromFuthark d
    e' <- fromFuthark e
    f' <- fromFuthark f
    g' <- fromFuthark g
    return (a', b', c', d', e', f', g')


toFutharkT2 (a, b) = do
    a' <- toFuthark a
    b' <- toFuthark b
    return (a', b')

toFutharkT3 (a, b, c) = do
    a' <- toFuthark a
    b' <- toFuthark b
    c' <- toFuthark c
    return (a', b', c')

toFutharkT4 (a, b, c, d) = do
    a' <- toFuthark a
    b' <- toFuthark b
    c' <- toFuthark c
    d' <- toFuthark d
    return (a', b', c', d')

toFutharkT5 (a, b, c, d, e) = do
    a' <- toFuthark a
    b' <- toFuthark b
    c' <- toFuthark c
    d' <- toFuthark d
    e' <- toFuthark e
    return (a', b', c', d', e')

toFutharkT6 (a, b, c, d, e, f) = do
    a' <- toFuthark a
    b' <- toFuthark b
    c' <- toFuthark c
    d' <- toFuthark d
    e' <- toFuthark e
    f' <- toFuthark f
    return (a', b', c', d', e', f')

toFutharkT7 (a, b, c, d, e, f, g) = do
    a' <- toFuthark a
    b' <- toFuthark b
    c' <- toFuthark c
    d' <- toFuthark d
    e' <- toFuthark e
    f' <- toFuthark f
    g' <- toFuthark g
    return (a', b', c', d', e', f', g')

|]

{--
instance (Output a a', Output b b')
  => Output (a, b) (a', b') where
    fromFuthark (a, b) = do
        a' <- fromFuthark a
        b' <- fromFuthark b
        return (a', b')

instance (Output a a', Output b b', Output c c')
  => Output (a, b, c) (a', b', c') where
    fromFuthark (a, b, c) = do
        a' <- fromFuthark a
        b' <- fromFuthark b
        c' <- fromFuthark c
        return (a', b', c')

instance (Output a a', Output b b', Output c c', Output d d')
  => Output (a, b, c, d) (a', b', c', d') where
    fromFuthark (a, b, c, d) = do
        a' <- fromFuthark a
        b' <- fromFuthark b
        c' <- fromFuthark c
        d' <- fromFuthark d
        return (a', b', c', d')

instance (Output a a', Output b b', Output c c', Output d d', Output e e')
  => Output (a, b, c, d, e) (a', b', c', d', e') where
    fromFuthark (a, b, c, d, e) = do
        a' <- fromFuthark a
        b' <- fromFuthark b
        c' <- fromFuthark c
        d' <- fromFuthark d
        e' <- fromFuthark e
        return (a', b', c', d', e')


instance (Input a a', Input b b')
  => Input (a, b) (a', b') where
    toFuthark (a, b) = do
        a' <- toFuthark a
        b' <- toFuthark b
        return (a', b')

instance (Input a a', Input b b', Input c c')
  => Input (a, b, c) (a', b', c') where
    toFuthark (a, b, c) = do
        a' <- toFuthark a
        b' <- toFuthark b
        c' <- toFuthark c
        return (a', b', c')

instance (Input a a', Input b b', Input c c', Input d d')
  => Input (a, b, c, d) (a', b', c', d') where
    toFuthark (a, b, c, d) = do
        a' <- toFuthark a
        b' <- toFuthark b
        c' <- toFuthark c
        d' <- toFuthark d
        return (a', b', c', d')

instance (Input a a', Input b b', Input c c', Input d d', Input e e')
  => Input (a, b, c, d, e) (a', b', c', d', e') where
    toFuthark (a, b, c, d, e) = do
        a' <- toFuthark a
        b' <- toFuthark b
        c' <- toFuthark c
        d' <- toFuthark d
        e' <- toFuthark e
        return (a', b', c', d', e')
--}


