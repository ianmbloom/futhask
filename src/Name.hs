{-# LANGUAGE OverloadedStrings #-}

module Name

where

import Data.Maybe
import Data.List (intercalate, partition, lookup)
import Data.Char (toUpper)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.String (IsString(..))

import Type
import GHC.SourceGen

primTable :: [(Text, Text)]
primTable =
    [ ("f32", "Float" )
    , ("f64", "Double")
    , ("bool","CBool" )
    , ("i8" , "Int8"  )
    , ("i16", "Int16" )
    , ("i32", "Int32" )
    , ("i64", "Int64" )
    , ("u8" , "Word8" )
    , ("u16", "Word16")
    , ("u32", "Word32")
    , ("u64", "Word64")
    ]

maybePrim :: Text -> Maybe Text
maybePrim key = lookup key primTable

futharkPrefix :: (FutharkType -> String) -> FutharkType -> String
futharkPrefix f ty =
  if isPrim ty
  then f ty
  else "futark_" <> f ty

mightApplySkolem :: (FutharkType -> RdrNameStr) -> FutharkType -> HsType'
mightApplySkolem f ty =
  let v = var . f $ ty
  in  if isPrim ty
      then v
      else v @@ var "c"

up :: (IsString a) => String -> a
up = fromString

prime :: (Semigroup a, IsString a) => a -> a
prime i            = i <> "'"

capitalize :: String -> String
capitalize string = toUpper (head string):tail string

raw :: ModuleNameStr
raw = "Raw"

entryApiName :: IsString a => FutharkEntry -> a
entryApiName entry = up $ futEntryName entry

entryRawName :: IsString a => FutharkEntry -> a
entryRawName entry = up $ "entry_"<> futEntryName entry

entryQualRawName :: FutharkEntry -> RdrNameStr
entryQualRawName = qual raw . entryRawName

entryCCallName :: IsString a => FutharkEntry -> a
entryCCallName entry = up $ "futhark_entry_" <> futEntryName entry

typeRawName :: IsString a => FutharkType -> a
typeRawName = up . capitalize . futharkPrefix toHaskellType

typeApiName :: IsString a => FutharkType -> a
typeApiName = up . toHaskellType

constructorName :: IsString a => FutharkType -> a
constructorName = up . capitalize . toHaskellType
