module Type

where

import qualified Data.Map as M
import Data.Maybe

data Backend = C | OpenCL | Cuda
  deriving (Eq, Ord, Show)

type TypeName        = String
type EntryName       = String
type PName           = String
type HaskellTypeName = String

data FutharkType
    = Prim
      { tyPrim     :: FutPrimType
      }
    | Array
      { tyElem :: FutPrimType
      , tyRank :: Int
      }
    | Opaque
      { tyOpaqueBase :: String
      }

data FutharkParameter =
    FParam
    { pName   :: PName
    , pUnique :: Bool
    , pType   :: FutharkType
    }

data FutharkEntry =
    FutharkEntry
    { futEntryName      :: EntryName
    , futEntryInParams  :: [FutharkParameter]
    , futEntryOutParams :: [FutharkParameter]
    }

isPrim :: FutharkType -> Bool
isPrim param =
  case param of
    Prim {} -> True
    _       -> False

buildArrayName :: FutharkType -> String
buildArrayName ty =
  let elemType = show . tyElem $ ty
      rank     = tyRank ty
  in  elemType <> "_" <> (show rank) <> "d"

toHaskellType :: FutharkType -> HaskellTypeName
toHaskellType ty =
  case ty of
    Prim   {} -> futPrimToHask . tyPrim $ ty
    Array  {} -> buildArrayName ty
    Opaque {} -> tyOpaqueBase ty

futPrimToHask :: FutPrimType -> HaskellTypeName
futPrimToHask futTy = show . fromJust . lookup futTy $ typeTable

mkFutType :: M.Map String FutharkType -> String -> FutharkType
mkFutType mapping name =
  case checkIsPrim name of
    Just prim -> Prim prim
    Nothing ->
      case M.lookup name mapping of
        Just found -> found
        Nothing -> error $ "Type " ++ name ++ " is not found."

data HaskPrimType
  = Hask_Float
  | Hask_Double
  | Hask_CBool
  | Hask_Int8
  | Hask_Int16
  | Hask_Int32
  | Hask_Int64
  | Hask_Word8
  | Hask_Word16
  | Hask_Word32
  | Hask_Word64
  deriving (Eq)

data FutPrimType
  = Fut_f32
  | Fut_f64
  | Fut_bool
  | Fut_i8
  | Fut_i16
  | Fut_i32
  | Fut_i64
  | Fut_u8
  | Fut_u16
  | Fut_u32
  | Fut_u64
  deriving (Eq)

instance Show HaskPrimType where
  show ty =
    case ty of
      Hask_Float  -> "Float"
      Hask_Double -> "Double"
      Hask_CBool  -> "CBool"
      Hask_Int8   -> "Int8"
      Hask_Int16  -> "Int16"
      Hask_Int32  -> "Int32"
      Hask_Int64  -> "Int64"
      Hask_Word8  -> "Word8"
      Hask_Word16 -> "Word16"
      Hask_Word32 -> "Word32"
      Hask_Word64 -> "Word64"

instance Show FutPrimType where
  show ty =
    case ty of
      Fut_f32  -> "f32"
      Fut_f64  -> "f64"
      Fut_bool -> "bool"
      Fut_i8   -> "i8"
      Fut_i16  -> "i16"
      Fut_i32  -> "i32"
      Fut_i64  -> "i64"
      Fut_u8   -> "u8"
      Fut_u16  -> "u16"
      Fut_u32  -> "u32"
      Fut_u64  -> "u64"

checkIsPrim :: String -> Maybe FutPrimType
checkIsPrim futTy = lookup futTy $ futPrimTable

readFutPrimType :: String -> FutPrimType
readFutPrimType = fromJust . checkIsPrim

typeTable :: [(FutPrimType, HaskPrimType)]
typeTable =
  [ (Fut_f32 , Hask_Float  )
  , (Fut_f64 , Hask_Double )
  , (Fut_bool, Hask_CBool  )
  , (Fut_i8  , Hask_Int8   )
  , (Fut_i16 , Hask_Int16  )
  , (Fut_i32 , Hask_Int32  )
  , (Fut_i64 , Hask_Int64  )
  , (Fut_u8  , Hask_Word8  )
  , (Fut_u16 , Hask_Word16 )
  , (Fut_u32 , Hask_Word32 )
  , (Fut_u64 , Hask_Word64 )
  ]

futPrimTable =
  [(show Fut_f32 , Fut_f32 )
  , (show Fut_f64 , Fut_f64 )
  , (show Fut_bool, Fut_bool)
  , (show Fut_i8  , Fut_i8  )
  , (show Fut_i16 , Fut_i16 )
  , (show Fut_i32 , Fut_i32 )
  , (show Fut_i64 , Fut_i64 )
  , (show Fut_u8  , Fut_u8  )
  , (show Fut_u16 , Fut_u16 )
  , (show Fut_u32 , Fut_u32 )
  , (show Fut_u64 , Fut_u64 )
  ]
