
wrapIn context rawObject 
    = fmap wrapper 
    $ FC.newForeignPtr 
        (inContextWithError context $ \c -> freeFO c rawObject)
        rawObject

peekFree p = peek p >>= \v -> free p >> return v
peekFreeWrapIn context rawP 
    = peek rawP >>= wrapIn context >>= \fo -> F.free rawP >> return fo


instance Input c CBool Bool where 
    toFuthark b = return $ if b then fromInteger 1 else fromInteger 0

instance Output c CBool Bool where
    fromFuthark cb = return $ if cb /= fromInteger 0 then True else False


-- Ptr - Dim conversion
peekAndFreeArray n a = peekArray n a >>= a' -> free a >> return a'

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

to3d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1, d2, d3] -> M.Sz4 d0 d1 d2 d3)
       . fmap (fmap fromIntegral)
       . peekAndFreeArray 4

to3d f cP aP
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

from4d f cP eP (M.Sz3 d0 d1 d2 d3)    = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)
                                                (fromIntegral d2)
                                                (fromIntegral d3)

from5d f cP eP (M.Sz3 d0 d1 d2 d3 d4) = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)
                                                (fromIntegral d2)
                                                (fromIntegral d3)
                                                (fromIntegral d4)

instance (FutharkArray array dim element)
    => Input c array (M.Array M.S dim element) where
    toFuthark array = unsafeLiftFromIO $ \context
    -> inContext context $ \c
    -> MU.unsafeWithPtr array (\aP -> new c aP $ M.size array)
    >>= wrapIn context

instance (FutharkArray array dim element)
    => Output c array (M.Array M.S dim element) where
    fromFuthark = unsafeLiftFromIO $ \context
    -> inContext context $ \c
    -> withFO array $ \aP
    -> do
        shape' <- shape c aP
        pointer <- mallocForeignPtrArray.M.totalElem shape'
        withForeignPtr pointer $ valuesFA cP aP
        return $ M.resize' shape
               $ MU.unsafeArrayFromForeignPtr0 M.Par pointer
               $ M.Sz1 (M.totalElem shape')

