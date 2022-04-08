{-# LANGUAGE QuasiQuotes #-}


module CodeBodies where
import Text.RawString.QQ
import Type

-- withFO :: FutharkObject wrapped raw => wrapped -> (Ptr raw -> IO b) -> IO b
-- withFO = withForeignPtr . snd . fromFO

typeClassesBody = [r|
class FutharkObject wrapped raw | wrapped -> raw, raw -> wrapped where
    wrapFO :: MVar Int -> ForeignPtr raw -> wrapped
    freeFO :: Ptr Raw.Futhark_context -> Ptr raw -> IO Int
    fromFO :: wrapped -> (MVar Int, ForeignPtr raw)

withFO :: FutharkObject wrapped raw => wrapped -> (Ptr raw -> IO b) -> IO b
withFO fo f = do
  let (mCounter, ptr) = fromFO fo
  counter <- readMVar mCounter
  if counter >= 0
  then withForeignPtr ptr f
  else error $ "withFO on object with < zero references."

debugFO :: FutharkObject wrapped raw => wrapped -> IO ()
debugFO fo = do
  let (mCounter, ptr) = fromFO fo
  counter <- readMVar mCounter
  putStrLn $ show counter

addReferenceFO :: (MonadIO m, FutharkObject wrapped raw) => wrapped -> FutT m ()
addReferenceFO fo = liftIO $
    let (referenceCounter, _) = fromFO fo
     in modifyMVar_ referenceCounter (\r -> pure (r+1))

finalizeFO :: (MonadIO m, FutharkObject wrapped raw) => wrapped -> FutT m ()
finalizeFO fo = liftIO $
    let (referenceCounter, pointer) = fromFO fo
    in modifyMVar_ referenceCounter (\r
     -> do if r == 0
           then finalizeForeignPtr pointer
           else when (r < 0) $ error $ "finalizing futhark object with less than zero references."
           return (r-1)
        )

class (FutharkObject array rawArray, Storable element, M.Index dim)
    => FutharkArray array rawArray dim element
    | array -> dim, array -> element
    where
        shapeFA  :: Ptr Raw.Futhark_context -> Ptr rawArray -> IO (M.Sz dim)
        newFA    :: Ptr Raw.Futhark_context -> Ptr element -> M.Sz dim -> IO (Ptr rawArray)
        valuesFA :: Ptr Raw.Futhark_context -> Ptr rawArray -> Ptr element -> IO Int

class Input fo ho where
    toFuthark :: MonadIO m => ho -> FutT m fo

class Output fo ho where
    fromFuthark     :: MonadIO m => fo -> FutT m ho

class HasShape fo dim where
    futharkShape :: MonadIO m => fo -> FutT m (M.Sz dim)

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
    (Size name s)        -> withCString name $ \n -> Raw.context_config_set_tuning_param config n s
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
    (Size name s)        -> withCString name $ \n -> Raw.context_config_set_tuning_param config n s
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
    childCount <- newMVar 0
    fmap (Context childCount)
        $ FC.newForeignPtr context
        $ (forkIO $ freeContext childCount config context)
        >> return ()

freeContext :: MVar Int
            -> Ptr Raw.Futhark_context_config
            -> Ptr Raw.Futhark_context
            -> IO ()
freeContext childCount config context
    = readMVar childCount >>= \n
    -> if n == 0
        then do Raw.context_free context
                Raw.context_config_free config
        else yield >> freeContext childCount config context

inContext :: Context -> (Ptr Raw.Futhark_context -> IO a) -> IO a
inContext (Context _ fp) = withForeignPtr fp

getError :: Context -> IO ()
getError context = do
    cs <- inContext context Raw.context_get_error
    s <- peekCString cs
    F.free cs
    error s

clearError :: Context -> IO ()
clearError context = inContext context Raw.context_get_error >>= F.free

clearCache :: Context -> IO ()
clearCache context
    = inContext context Raw.context_clear_caches >>= \code
    -> if code == 0
        then return ()
        else getError context

syncContext :: Context -> IO ()
syncContext context
    = inContext context Raw.context_sync >>= \code
    -> if code == 0
        then return ()
        else getError context

inContextWithError :: Context -> (Ptr Raw.Futhark_context -> IO Int) -> IO ()
inContextWithError context f = do
    code <- attempt
    if code == 0
        then success
        else do
            clearError context
            performGC
            code' <- attempt
            if code' == 0
                then success
                else failure
    where
        attempt = inContext context f
        success = return ()
        failure = getError context

|]

futBody useLinear =
  if useLinear
  then
  [r|

type FutT m = StateT Context m
type FutIO = FutT Linear.IO

class MonadIO m => MonadFut m where
    liftFut :: FutIO a %1 -> m a

instance (MonadFut m) => MonadFut (StateT s m) where
  liftFut = lift . liftFut

instance {-# OVERLAPPING #-} MonadFut FutIO where
  liftFut = id

runFutTIn :: Monad m => Context -> FutT m a -> m a
runFutTIn context f = do (x, context') <- runStateT f context
                         return x

runFutInside :: (Monad m0, Monad m1) => FutT m1 a -> Context %1 -> m0 (m1 a)
runFutInside a c = return $ runFutTIn c a

runFutTWith :: Monad m => [ContextOption] -> FutT m a -> m a
runFutTWith options a
    = fromSystemIO
    $ P.unsafePerformIO
    $ getContext options >>= runFutInside a
runFutT :: Monad m => FutT m a -> m a
runFutT = runFutTWith []

unsafeLiftFromIO :: forall a m . (MonadIO m) => (Context %1 -> Linear.IO a) -> FutT m a
unsafeLiftFromIO a = do
  context <- get
  x <- liftIO (a context :: Linear.IO a)
  put context
  return x

|]
  else
  [r|
type FutT m = StateT Context m
type Fut = FutT Identity
type FutIO = FutT IO

class MonadIO m => MonadFut m where
    liftFut :: FutIO a -> m a

instance (MonadFut m) => MonadFut (StateT s m) where
  liftFut = lift . liftFut

instance {-# OVERLAPPING #-} MonadFut FutIO where
  liftFut = id

runFutTIn :: Monad m => Context -> FutT m a -> m a
runFutTIn context a = evalStateT a context

runFutTWith :: Monad m => [ContextOption] -> FutT m a -> m a
runFutTWith options a
    = unsafePerformIO
    $ getContext options >>= \c -> return $ runFutTIn c a
runFutT :: Monad m => FutT m a -> m a
runFutT = runFutTWith []

runFutIn :: Context -> Fut a -> a
runFutIn context a = runIdentity $ runFutTIn context $ a

runFutWith :: [ContextOption] -> Fut a -> a
runFutWith options a = runIdentity $ runFutTWith options a
runFut = runFutWith []

unsafeFromFutIO :: forall a . FutIO a -> Fut a
unsafeFromFutIO a = do
  context <- get
  let (x, context') = unsafePerformIO $ (runStateT a context :: IO (a, Context))
  put context'
  return x

unsafeLiftFromIO :: forall a m . (MonadIO m) => (Context -> IO a) -> FutT m a
unsafeLiftFromIO a = do
  context <- get
  x <- liftIO (a context :: IO a)
  put context
  return x

|]

wrapBody = [r|

wrapIn :: FutharkObject wrapped raw => Context -> Ptr raw -> IO wrapped
wrapIn context@(Context childCount pointer) rawObject = do
    modifyMVar_ childCount (\cc -> return $! (cc+1))
    referenceCounter <- newMVar 0
    pointer <- FC.newForeignPtr rawObject freeCall
    pure $! wrapFO referenceCounter pointer
    where freeCall = (inContextWithError context $ \c -> freeFO c rawObject)
                  >> modifyMVar_ childCount (\cc -> return $! (cc-1))

peekFree :: (Storable b) => Ptr b -> IO b
peekFree p = do
   pPeeked <- peek p
   free p
   return pPeeked

peekFreeWrapIn :: (FutharkObject b raw) => Context -> Ptr (Ptr raw) -> IO b
peekFreeWrapIn context rawP = do
   rawPeeked <- peek rawP
   wrapped <- wrapIn context rawPeeked
   F.free rawP
   return wrapped

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
          syncContext context
          shape <- shapeFA c aP
          pointer <- mallocForeignPtrArray $ M.totalElem shape
          withForeignPtr pointer $ valuesFA c aP
          return $ M.resize' shape
                 $ MU.unsafeArrayFromForeignPtr0 M.Seq pointer
                 $ M.Sz1 (M.totalElem shape)

instance (FutharkArray array rawArray dim element)
  => HasShape array dim where
    futharkShape array = unsafeLiftFromIO $ \context
      -> inContext context $ \c
      -> withFO array $ \aP
      -> do
          shape <- shapeFA c aP
          return shape

boolToCBool :: Bool -> CBool
boolToCBool True  = CBool 1
boolToCBool False = CBool 0

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
