module Type

where

data Backend = C | OpenCL | Cuda

type TypeName = String
type EntryName = String

data HaskType
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

instance Show HaskType where
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

data FutType
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

instance Show FutType where
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

typeTable :: [(FutType, HaskType)]
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

futToHask :: FutType -> HaskType
futToHask = undefined
