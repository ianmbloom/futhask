{-# LANGUAGE MultiParamTypeClasses #-}
module TestModule.TypeClasses (FutharkObject, FutharkArray, Input, Output) where
import qualified TestModule.Raw as Raw

class FutharkObject wrapped raw | wrapped -> raw where
    wrapper :: ForeignPtr raw -> wrapped
    freeFO :: Ptr Raw.Futhark_context -> Ptr raw -> IO Int
    withFO :: wrapped -> (Ptr raw -> IO b) -> IO b
    


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

