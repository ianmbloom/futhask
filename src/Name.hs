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

mightApplySkolem :: (FutharkType -> RdrNameStr) -> FutharkType -> HsType'
mightApplySkolem f ty =
  let v = var . f $ ty
  in  if isPrim ty
      then v
      else v @@ var "c"

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

opCName :: IsString a => String -> FutharkType -> a
opCName op ty = up $ op <> "_" <> mightOpaque toHaskellType ty

newCName      :: IsString a => FutharkType -> a
freeCName     :: IsString a => FutharkType -> a
valuesCName   :: IsString a => FutharkType -> a
shapeCName    :: IsString a => FutharkType -> a
storeCName    :: IsString a => FutharkType -> a
restoreCName  :: IsString a => FutharkType -> a
newCName     = opCName "new"
freeCName    = opCName "free"
valuesCName  = opCName "values"
shapeCName   = opCName "shape"
storeCName   = opCName "store"
restoreCName = opCName "restore"
