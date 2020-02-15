{-# MultiParamTypeClasses #-}

module Objects where

import qualified Raw
import Context
import FT
import qualified Data.Massiv.Array as M

class FutharkObject wrapped raw | wrapped -> raw where
    wrapper :: ForeignPtr raw -> wrapped
    freeFO :: Ptr Raw.Futhark_context -> Ptr raw -> IO Int
    withFO :: wrapped -> (Ptr raw -> IO b) -> IO b
    

wrapIn context rawObject 
    = fmap wrapper 
    $ FC.newForeignPtr 
        (inContextWithError context $ \c -> freeFO c rawObject)
        rawObject

peekAndWrapIn context rawP 
    = peek rawP >>= wrapIn context >>= \fo -> F.free rawP >> return fo


class (FutharkObject array rawArray, Storable element, Index dim) 
    => FutharkArray array dim element 
    | array -> dim, array -> element 
    where
        shape :: Ptr Raw.Futhark_context -> Ptr rawArray -> IO dim 
        new :: Ptr Raw.Futhark_context -> Ptr element -> dim -> IO (Ptr rawArray)
        values :: Ptr Raw.Futhark_context -> Ptr rawArray -> Ptr element -> IO Int 

class Input c fo ho where
    toFuthark :: ho -> FT c fo

class Output c fo ho where
    fromFuthark :: fo -> FT c ho

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

apply1d f cP eP (M.Sz1 d0)             = f cP eP (fromIntegral d0)

apply2d f cP eP (M.Sz2 d0 d1)          = f cP eP (fromIntegral d0)
                                                 (fromIntegral d1)

apply3d f cP eP (M.Sz3 d0 d1 d2)       = f cP eP (fromIntegral d0)
                                                 (fromIntegral d1)
                                                 (fromIntegral d2)

apply4d f cP eP (M.Sz3 d0 d1 d2 d3)    = f cP eP (fromIntegral d0)
                                                 (fromIntegral d1)
                                                 (fromIntegral d2)
                                                 (fromIntegral d3)

apply5d f cP eP (M.Sz3 d0 d1 d2 d3 d4) = f cP eP (fromIntegral d0)
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


{--
instance (Functor f, Output a) => Output (f a) where
    fromFuthark = fmap fromFuthark


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
--}
