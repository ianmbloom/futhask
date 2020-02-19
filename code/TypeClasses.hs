
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

