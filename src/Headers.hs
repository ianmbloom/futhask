
module Headers where
import Data.List (intercalate)
import Backends

data Import = N String | Q String String

globalImport (N m) = "import " ++ m ++ "\n"
globalImport (Q m a) = "import qualified " ++ m ++ " as " ++ a ++ "\n"
localImport moduleName (N sub) = globalImport $ N (moduleName ++ "." ++ sub)
localImport moduleName (Q sub a) = globalImport $ Q (moduleName ++ "." ++ sub) a

haskellHeader exports extensions localImports globalImports moduleName subModuleName 
    =  (if length extensions > 0 
        then "{-# LANGUAGE " ++ intercalate ", " extensions ++ " #-}" 
        else "")
    ++ "\nmodule " 
    ++ moduleName ++ (case subModuleName of Nothing -> ""; Just n -> '.':n)
    ++ (if length exports > 0 then " (" ++ intercalate ", " exports ++ ")" else "")
    ++ " where\n"
    ++ concatMap (localImport moduleName) localImports
    ++ concatMap globalImport globalImports

specific C = []
specific OpenCL = [N "Control.Parallel.OpenCL (CLMem, CLCommandQueue)"]
specific Cuda = [N "Foreign.CUDA.Ptr(DevicePtr(..))"]

rawHeader backend = haskellHeader
    []
    [ "ForeignFunctionInterface" ]
    [] 
    ( [ N "Data.Int (Int8, Int16, Int32, Int64)"
      , N "Data.Word (Word8, Word16, Word32, Word64)"
      , N "Foreign.C.Types (CBool(..), CSize(..), CChar(..))"
      , N "Foreign.Ptr (Ptr)" ] ++ specific backend )

typeClassesHeader backend = haskellHeader
    [ "FutharkObject", "FutharkArray"
    , "freeFO", "fromFO", "withFO", "wrapFO", "finalizeFO"
    , "newFA", "shapeFA", "valuesFA"
    , "Input", "Output"
    , "fromFuthark", "toFuthark" ]
    [ "MultiParamTypeClasses", "FunctionalDependencies", "TypeSynonymInstances" ]
    [ Q "Raw" "Raw", N "Fut" ] 
    [ N "Foreign", Q "Data.Massiv.Array" "M"
    , N "Control.Monad.Trans", N "Control.Monad.IO.Class", N "Control.Concurrent" ]

configHeader backend = haskellHeader
    []
    []
    [ Q "Raw" "Raw" ]
    ( [ N "Foreign.C" ] ++ specific backend )

contextHeader backend = haskellHeader
    []
    []
    [Q "Raw" "Raw", N "Config" ]
    [N "Foreign as F", Q "Foreign.Concurrent" "FC", N "Foreign.C", N "Control.Concurrent", N "System.Mem (performGC)"]

futHeader backend = haskellHeader
    [ "FutT", "Fut", "FutIO"
    , "runFutIn", "runFutWith", "runFut", "runFutTIn", "runFutTWith", "runFutT"
    , "mapFutT", "map2FutT", "pureFut", "unsafeFromFutIO", "unsafeLiftFromIO" ]
    [ "RankNTypes", "ExistentialQuantification", "FlexibleInstances", "UndecidableInstances", "TypeFamilies", "MultiParamTypeClasses" ]
    [ N "Context", N "Config" ]
    [ N "System.IO.Unsafe", N "Control.Monad.Base", N "Control.Monad.Trans", N "Control.Monad.Trans.Control", N "Control.Monad.Identity" ]

wrapHeader backend = haskellHeader
    []
    [ "RankNTypes"
    , "FlexibleInstances"
    , "FlexibleContexts"
    , "MultiParamTypeClasses"
    , "UndecidableInstances" ]
    [ Q "Raw" "Raw", N "Context", N "Fut", N "TypeClasses" ]
    [ N "Foreign as F", Q "Foreign.Concurrent" "FC", N "Foreign.C"

    , Q "Data.Massiv.Array" "M", Q "Data.Massiv.Array.Unsafe" "MU", N "Control.Concurrent" ]

typesHeader backend = haskellHeader
    []
    [ "RankNTypes", "ExistentialQuantification"
    , "MultiParamTypeClasses", "TypeSynonymInstances", "FlexibleInstances" ]
    [ Q "Raw" "Raw", N "Wrap", N "TypeClasses" ]
    [ Q "Foreign" "F", Q "Data.Massiv.Array" "M"
    , Q "Control.Concurrent.MVar" "MV"
    , N "Data.Int (Int8, Int16, Int32, Int64)"
    , N "Data.Word (Word8, Word16, Word32, Word64)"
    , N "Foreign.C.Types (CBool(..), CSize(..), CChar(..))"
    , N "Foreign.Ptr (Ptr)"
    , N "Control.DeepSeq (rwhnf)" ]

entriesHeader backend = haskellHeader 
    []
    []
    [ Q "Raw" "Raw", Q "Context" "C", N "Fut (FutT)", Q "Fut" "Fut"
    , Q "Wrap" "U", N "Types", Q "TypeClasses" "T" ]
    [ N "Data.Int (Int8, Int16, Int32, Int64)"
    , N "Data.Word (Word8, Word16, Word32, Word64)" 
    , Q "Foreign" "F", N "Foreign.C.Types" ]

utilsHeader backend = haskellHeader
    []
    [ "RankNTypes"
    , "FlexibleContexts"
    , "FlexibleInstances"
    , "MultiParamTypeClasses"
    , "UndecidableInstances" ]
    [ Q "Raw" "Raw", N "Context", N "Fut", N "TypeClasses", N "Wrap" ]
    [ N "Foreign as F", Q "Foreign.Concurrent" "FC", N "Foreign.C"

    , Q "Data.Massiv.Array" "M", Q "Data.Massiv.Array.Unsafe" "MU" ]

exportsHeader backend = haskellHeader
    [ "module F" ]
    []
    [ N "Context as F"
    , N "Config as F hiding (setOption)"
    , N "TypeClasses as F hiding (FutharkObject, FutharkArray)"
    , N "Utils as F"
    , N "Fut as F" ]
    []
