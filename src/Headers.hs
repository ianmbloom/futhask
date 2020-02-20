
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
    , "freeFO", "withFO", "wrapFO", "newFA", "shapeFA", "valuesFA"
    , "Input", "Output"
    , "fromFuthark", "toFuthark" ]
    [ "MultiParamTypeClasses", "FunctionalDependencies" ]
    [ Q "Raw" "Raw", N "FT" ] 
    [ N "Foreign", Q "Data.Massiv.Array" "M" ]

configHeader backend = haskellHeader
    []
    []
    [ Q "Raw" "Raw" ]
    ( [ N "Foreign.C" ] ++ specific backend )

contextHeader backend = haskellHeader
    []
    []
    [Q "Raw" "Raw", N "Config" ]
    [N "Foreign as F", Q "Foreign.Concurrent" "FC", N "Foreign.C" ]

fTHeader backend = haskellHeader
    [ "FT", "runFTIn", "runFTWith", "runFT", "unsafeLiftFromIO" ]
    [ "RankNTypes", "ExistentialQuantification" ]
    [ N "Context", N "Config" ]
    [ N "System.IO.Unsafe" ]

utilsHeader backend =haskellHeader
    []
    [ "RankNTypes"
    , "FlexibleInstances"
    , "MultiParamTypeClasses"
    , "UndecidableInstances" ]
    [ Q "Raw" "Raw", N "Context", N "FT", N "TypeClasses" ]
    [ N "Foreign as F", Q "Foreign.Concurrent" "FC", N "Foreign.C"
    , Q "Data.Massiv.Array" "M", Q "Data.Massiv.Array.Unsafe" "MU" ]

typesHeader backend = haskellHeader
    []
    [ "RankNTypes", "ExistentialQuantification"
    , "MultiParamTypeClasses", "TypeSynonymInstances", "FlexibleInstances" ]
    [ Q "Raw" "Raw", N "Utils", N "TypeClasses" ]
    [ Q "Foreign" "F", Q "Data.Massiv.Array" "M"
    , N "Data.Int (Int8, Int16, Int32, Int64)"
    , N "Data.Word (Word8, Word16, Word32, Word64)"
    , N "Foreign.C.Types (CBool(..), CSize(..), CChar(..))"
    , N "Foreign.Ptr (Ptr)" ]

entriesHeader backend = haskellHeader
    []
    []
    [ Q "Raw" "Raw", Q "Context" "C", N "FT (FT)", Q "FT" "FT"
    , Q "Utils" "U", N "Types", Q "TypeClasses" "T" ]
    [ N "Data.Int (Int8, Int16, Int32, Int64)"
    , N "Data.Word (Word8, Word16, Word32, Word64)" 
    , Q "Foreign" "F", N "Foreign.C.Types" ]

exportsHeader backend = haskellHeader
    [ "module F" ]
    []
    [ N "Context as F"
    , N "Config as F hiding (setOption)"
    , N "TypeClasses as F hiding (FutharkObject, FutharkArray)"
    , N "Utils as F ()"
    , N "FT as F" ]
    []
