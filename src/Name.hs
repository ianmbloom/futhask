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

mightFutharkPrefix :: (FutharkType -> String) -> FutharkType -> String
mightFutharkPrefix f ty =
  if isPrim ty
  then f ty
  else "futark_" <> f ty

mightOpaque :: (FutharkType -> String) -> FutharkType -> String
mightOpaque f ty =
  case ty of
    Opaque {} -> "opaque_" <> f ty
    _ -> f ty

up :: (IsString a) => String -> a
up = fromString

prime :: (Semigroup a, IsString a) => a -> a
prime i = i <> "'"

capitalize :: String -> String
capitalize string = toUpper (head string):tail string

raw :: ModuleNameStr
raw = "Raw"

entryApiName :: IsString a => FutharkEntry -> a
entryApiName entry = up $ futEntryName entry

entryRawName :: IsString a => FutharkEntry -> a
entryRawName entry = up $ "entry_" <> futEntryName entry

entryQualRawName :: FutharkEntry -> RdrNameStr
entryQualRawName = qual raw . entryRawName

typeRawName :: IsString a => FutharkType -> a
typeRawName = up . capitalize . mightFutharkPrefix toHaskellType

typeApiName :: IsString a => FutharkType -> a
typeApiName = up . toHaskellType

constructorName :: IsString a => FutharkType -> a
constructorName = up . capitalize . toHaskellType

apiNameOp :: IsString a => String -> FutharkType -> a
apiNameOp op ty = up $ op <> "_" <> toHaskellType ty

apiNameNew      :: IsString a => FutharkType -> a
apiNameFree     :: IsString a => FutharkType -> a
apiNameValues   :: IsString a => FutharkType -> a
apiNameShape    :: IsString a => FutharkType -> a
apiNameStore    :: IsString a => FutharkType -> a
apiNameRestore  :: IsString a => FutharkType -> a
apiNameNew     = apiNameOp "new"
apiNameFree    = apiNameOp "free"
apiNameValues  = apiNameOp "values"
apiNameShape   = apiNameOp "shape"
apiNameStore   = apiNameOp "store"
apiNameRestore = apiNameOp "restore"

cNameOp :: IsString a => String -> FutharkType -> a
cNameOp op ty = up $ op <> "_" <> mightOpaque toHaskellType ty

cNameNew      :: IsString a => FutharkType -> a
cNameFree     :: IsString a => FutharkType -> a
cNameValues   :: IsString a => FutharkType -> a
cNameShape    :: IsString a => FutharkType -> a
cNameStore    :: IsString a => FutharkType -> a
cNameRestore  :: IsString a => FutharkType -> a
cNameNew     = cNameOp "new"
cNameFree    = cNameOp "free"
cNameValues  = cNameOp "values"
cNameShape   = cNameOp "shape"
cNameStore   = cNameOp "store"
cNameRestore = cNameOp "restore"
