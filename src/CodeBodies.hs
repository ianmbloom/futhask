{-# LANGUAGE QuasiQuotes #-}


module CodeBodies where
import Text.RawString.QQ
import Type

typeClassesBody = [r|
class FutharkObject wrapped raw | wrapped -> raw, raw -> wrapped where
    wrapFO :: MVar Int -> ForeignPtr raw -> wrapped c
    freeFO :: Ptr Raw.Futhark_context -> Ptr raw -> IO Int
    fromFO :: wrapped c -> (MVar Int, ForeignPtr raw)

withFO :: FutharkObject wrapped raw => wrapped c -> (Ptr raw -> IO b) -> IO b
withFO = withForeignPtr . snd . fromFO

addReferenceFO :: (MonadIO m, FutharkObject wrapped raw) => wrapped c -> FutT c m ()
addReferenceFO fo = liftIO $
    let (referenceCounter, _) = fromFO fo
     in modifyMVar_ referenceCounter (\r -> pure (r+1))

finalizeFO :: (MonadIO m, FutharkObject wrapped raw) => wrapped c -> FutT c m ()
finalizeFO fo = liftIO $
    let (referenceCounter, pointer) = fromFO fo
     in modifyMVar_ referenceCounter (\r
     -> if r > 0
            then pure (r-1)
            else finalizeForeignPtr pointer >> pure 0)


class (FutharkObject array rawArray, Storable element, M.Index dim)
    => FutharkArray array rawArray dim element
    | array -> dim, array -> element
    where
        shapeFA  :: Ptr Raw.Futhark_context -> Ptr rawArray -> IO (M.Sz dim)
        newFA    :: Ptr Raw.Futhark_context -> Ptr element -> M.Sz dim -> IO (Ptr rawArray)
        valuesFA :: Ptr Raw.Futhark_context -> Ptr rawArray -> Ptr element -> IO Int

class Input fo ho where
    toFuthark :: Monad m => ho -> FutT c m (fo c)

class Output fo ho where
    fromFuthark :: Monad m => fo c -> FutT c m ho

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

futBody = [r|

newtype FutT c m a = FutT (Context -> m a)

instance MonadTrans (FutT c) where
    lift a = FutT (\_ -> a)
    {-# INLINEABLE lift #-}

instance Functor m => Functor (FutT c m) where
    fmap f (FutT a) = FutT (fmap f.a)
    {-# INLINEABLE fmap #-}

instance Applicative m => Applicative (FutT c m) where
    pure a = FutT (\_ -> pure a)
    (<*>) (FutT a) (FutT b) = FutT (\c -> a c <*> b c)
    {-# INLINEABLE pure #-}
    {-# INLINEABLE (<*>) #-}

instance Monad m => Monad (FutT c m) where
    (>>=) (FutT a) f = FutT (\c -> a c >>= (\(FutT b) -> b c) . f)
    {-# INLINEABLE (>>=) #-}

instance MonadIO m => MonadIO (FutT c m) where
   liftIO = lift . liftIO
   {-# INLINEABLE liftIO #-}

instance (MonadBase b m) => MonadBase b (FutT c m) where
    liftBase = liftBaseDefault
    {-# INLINEABLE liftBase #-}

instance MonadTransControl (FutT c) where
    type StT (FutT c) a = a
    liftWith a = FutT (\c -> a (\(FutT a') -> a' c))
    restoreT = lift
    {-# INLINEABLE liftWith #-}
    {-# INLINEABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (FutT c m) where
    type StM (FutT c m) a = ComposeSt (FutT c) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
    {-# INLINEABLE liftBaseWith #-}
    {-# INLINEABLE restoreM #-}


type Fut c = FutT c Identity
type FutIO c = FutT c IO

mapFutT :: (m a -> n b) -> FutT c m a -> FutT c n b
mapFutT f (FutT a) = FutT (f.a)
map2FutT :: (m a -> n b -> k c) -> FutT c' m a -> FutT c' n b -> FutT c' k c
map2FutT f (FutT a) (FutT b) = FutT (\c -> f (a c) (b c))


runFutTIn :: Context -> (forall c. FutT c m a) -> m a
runFutTIn context (FutT a) = a context

runFutTWith :: [ContextOption] -> (forall c. FutT c m a) -> m a
runFutTWith options a
    = unsafePerformIO
    $ getContext options >>= \c -> return $ runFutTIn c a
runFutT = runFutTWith []

runFutIn :: Context -> (forall c. Fut c a) -> a
runFutIn context a = runIdentity $ runFutTIn context $ a

runFutWith :: [ContextOption] -> (forall c. Fut c a) -> a
runFutWith options a = runIdentity $ runFutTWith options a
runFut = runFutWith []

pureFut :: (Monad m) => Fut c a -> FutT c m a
pureFut (FutT a) = FutT (pure . runIdentity . a)

unsafeFromFutIO :: FutIO c a -> Fut c a
unsafeFromFutIO (FutT a) = FutT (Identity . unsafePerformIO . a)

unsafeLiftFromIO :: Monad m => (Context -> IO a) -> FutT c m a
unsafeLiftFromIO a = FutT (pure . unsafePerformIO . a)

|]

wrapBody = [r|

wrapIn :: FutharkObject wrapped raw => Context -> Ptr raw -> IO (wrapped c)
wrapIn context@(Context childCount pointer) rawObject = do
    modifyMVar_ childCount (\cc -> return $! (cc+1))
    referenceCounter <- newMVar 0
    pointer <- FC.newForeignPtr rawObject freeCall
    pure $! wrapFO referenceCounter pointer
    where freeCall = (inContextWithError context $ \c -> freeFO c rawObject)
                  >> modifyMVar_ childCount (\cc -> return $! (cc-1))

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
          syncContext context
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
