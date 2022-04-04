
module Headers where
import Data.List (intercalate)
import Type

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

nonLinearImports :: [Import]
nonLinearImports =
  [ N "Control.Monad.Base"
  , N "Control.Monad.Trans"
  , N "Control.Monad.State"
  , N "Control.Monad.Catch"
  , N "Control.Monad.Trans.Control"
  , N "Control.Monad.Trans.Class"
  , N "Control.Monad.Identity"
  , N "Control.Monad.IO.Class"
  , N "System.IO.Unsafe"
  , N "Data.Functor.Identity"
  ]

linearImports :: [Import]
linearImports =
  [ N "Control.Functor.Linear"
  , Q "System.IO.Linear" "Linear"
  , N "System.IO.Linear hiding (IO)"
  , N "Control.Monad.IO.Class.Linear"
  , N "Data.Tuple.Linear"
  , Q "Prelude" "P"
  , Q "System.IO.Unsafe" "P"
  , Q "Data.Functor.Identity" "P"
  , N "Prelude.Linear hiding (snd)"
  , N "Control.Monad.Base"
  , N "Control.Monad.Catch"
  ]

monadImports :: Bool -> [Import]
monadImports useLinear = if useLinear then linearImports else nonLinearImports

rawHeader backend = haskellHeader
    []
    [ "ForeignFunctionInterface" ]
    []
    ( [ N "Data.Int (Int8, Int16, Int32, Int64)"
      , N "Data.Word (Word8, Word16, Word32, Word64)"
      , N "Foreign.C.Types (CBool(..), CSize(..), CChar(..), CFile(..))"
      , N "Foreign.Ptr (Ptr)"
      ] ++ specific backend
    )


typeClassesHeader useLinear backend = haskellHeader
    [ "FutharkObject", "FutharkArray"
    , "freeFO", "fromFO", "withFO", "wrapFO", "addReferenceFO", "finalizeFO"
    , "newFA", "shapeFA", "valuesFA"
    , "Input", "Output", "HasShape(..)"
    , "fromFuthark", "toFuthark" ]
    ([ "MultiParamTypeClasses"
     , "FunctionalDependencies"
     , "TypeSynonymInstances"
     ] ++ if useLinear then ["NoImplicitPrelude"] else []
    )
    [ Q "Raw" "Raw", N "Fut" ]
    ( [ N "Foreign", Q "Data.Massiv.Array" "M"
      , N "Control.Concurrent"
      ] ++ monadImports useLinear
    )

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

futHeader useLinear backend =
    haskellHeader
    [ "FutT", "Fut", "FutIO", "MonadFut(..)"
    , "runFutIn", "runFutWith", "runFut", "runFutTIn", "runFutTWith", "runFutT"
    , "mapFutT", "map2FutT", "pureFut", "unsafeFromFutIO", "unsafeLiftFromIO" ]
    ([ "RankNTypes", "ExistentialQuantification", "FlexibleInstances", "UndecidableInstances", "TypeFamilies", "MultiParamTypeClasses" ]
     ++ if useLinear then ["NoImplicitPrelude", "LinearTypes", "ApplicativeDo"] else []
    )
    [ N "Context", N "Config" ]
    ( [
      ] ++ monadImports useLinear
    )

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
    , N "Foreign.C.Types (CBool(..), CSize(..), CChar(..), CFile(..))"
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
    , N "TypeClasses as F"
    , N "Utils as F"
    , N "Fut as F"
    , N "Types as F"]
    []
