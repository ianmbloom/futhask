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
entryRawName entry = up $ "entry_" <> futEntryName entry

entryQualRawName :: FutharkEntry -> RdrNameStr
entryQualRawName = qual raw . entryRawName

typeRawName :: IsString a => FutharkType -> a
typeRawName = up . capitalize . mightFutharkPrefix toHaskellType

typeApiName :: IsString a => FutharkType -> a
typeApiName = up . toHaskellType

constructorName :: IsString a => FutharkType -> a
constructorName = up . capitalize . toHaskellType
