{-# LANGUAGE OverloadedStrings #-}

module Conversion where

import Data.Maybe
import Data.List (intercalate, partition, lookup)
import Data.Char (toUpper)
import Text.ParserCombinators.ReadP
import Manifest
import qualified Data.Map  as M
import Data.Map ((!))
import qualified Data.Text as T
import Data.Text (Text(..))
import Debug
import Data.String (IsString(..))

import Generate
import Name

buildArrayType :: Type -> Text
buildArrayType ty =
  let elemType = tyArrayElemType ty
      rank     = tyArrayRank     ty
  in  elemType <> "_" <> (T.pack $ show rank) <> "d"

typeToHaskell :: Manifest -> Text -> Text
typeToHaskell manifest key =
  let ty     = manifestTypes manifest ! key
  in  case maybePrim key of
        Just prim -> prim
        Nothing   ->
          case ty of
            TypeArray  {} -> buildArrayType ty
            TypeOpaque {} -> key

addPointerLevel :: Int -> Int -> (Text -> Text) -> Text -> Text
addPointerLevel primLevel arrayLevel f key =
  let mPrim  = maybePrim key
      level  =
        case mPrim of
          Just {} -> primLevel
          Nothing -> arrayLevel
  in  pointerLevel level (f key)

arrayNewCall :: Text -> Text -> Type -> [Text]
arrayNewCall elemTy haskellTy ty =
  let arrName     = "new_" <> buildArrayType ty
      arrayType   = pointerLevel 1 elemTy
      shapeParams = replicate (tyArrayRank ty) "Int64"
      returnType  = pointerLevel 1 haskellTy
  in  foreignImportCall arrName arrName (arrayType:shapeParams) returnType

arrayFreeCall :: Text -> Type -> [Text]
arrayFreeCall haskellTy ty =
  let arrName     = "free_" <> buildArrayType ty
      inputType   = pointerLevel 1 haskellTy
      returnType  = "Int"
  in  foreignImportCall arrName arrName [inputType] returnType

arrayValuesCall :: Text -> Text -> Type -> [Text]
arrayValuesCall elemTy haskellTy ty =
  let arrName     = "values_" <> buildArrayType ty
      inputType   = pointerLevel 1 haskellTy
      outputType  = pointerLevel 1 elemTy
      returnType  = "Int"
  in  foreignImportCall arrName arrName [inputType, outputType] returnType

arrayShapeCall :: Text -> Type -> [Text]
arrayShapeCall haskellTy ty =
  let arrName     = "shape_" <> buildArrayType ty
      inputTypes  = pointerLevel 1 haskellTy
      returnType  = pointerLevel 1 "Int64"
  in  foreignImportCall arrName arrName [inputTypes] returnType

opaqueFreeCall :: Text -> Text -> Type -> [Text]
opaqueFreeCall key haskellTy ty =
  let arrName    = "free_" <> key
      callName   = "free_opaque_" <> key
      inputTypes = [ pointerLevel 1 haskellTy
                   ]
      returnType = "Int"
  in  foreignImportCall arrName callName inputTypes returnType

opaqueStoreCall :: Text -> Text -> Type -> [Text]
opaqueStoreCall key haskellTy ty =
  let arrName    = "store_" <> key
      callName   = "store_opaque_" <> key
      inputTypes = [ pointerLevel 1 haskellTy
                   , pointerLevel 2 "()"
                   , pointerLevel 1 "CSize"
                   ]
      returnType = "Int"
  in  foreignImportCall arrName callName inputTypes returnType

opaqueRestoreCall :: Text -> Text -> Type -> [Text]
opaqueRestoreCall key haskellTy ty =
  let arrName    = "restore_" <> key
      callName   = "restore_opaque_" <> key
      inputTypes = [pointerLevel 1 "()"]
      returnType = pointerLevel 1 haskellTy
  in  foreignImportCall arrName callName inputTypes returnType

foreignTypeDeclarations :: Manifest -> Text -> [Text]
foreignTypeDeclarations manifest key =
    let ty = lookupType manifest key
        haskellTy = fromRaw (typeToHaskell manifest) key
        elemTy = typeToHaskell manifest . tyArrayElemType $ ty
    in
    foreignDataDeclaration haskellTy
    <>
    case ty of
      TypeArray {} ->
           arrayNewCall    elemTy haskellTy ty
        <> arrayFreeCall          haskellTy ty
        <> arrayValuesCall elemTy haskellTy ty
        <> arrayShapeCall         haskellTy ty
      TypeOpaque {} ->
           opaqueFreeCall    key haskellTy ty
        <> opaqueStoreCall   key haskellTy ty
        <> opaqueRestoreCall key haskellTy ty

lookupType :: Manifest -> Text -> Type
lookupType manifest  = (M.!) (manifestTypes manifest)

lookupEntry :: Manifest -> Text -> EntryPoint
lookupEntry manifest = (M.!) (manifestEntryPoints manifest)

foreignEntryDeclaration :: Manifest -> Text -> [Text]
foreignEntryDeclaration manifest key =
    let entry       = lookupEntry manifest key
        arrName     = "entry_" <> key
        outputTypes = map (addPointerLevel 1 2 (capitalize . fromRaw (typeToHaskell manifest)) . outputType) $ entryPointOutputs entry
        inputTypes  = map (addPointerLevel 0 1 (capitalize . fromRaw (typeToHaskell manifest)) . inputType ) $ entryPointInputs entry
        params      = outputTypes <> inputTypes
    in  foreignImportCall arrName arrName params "Int"

rawImportString :: Manifest -> [Text]
rawImportString manifest =
  concatMap (foreignEntryDeclaration manifest) (M.keys $ manifestEntryPoints manifest)
  ++
  concatMap (foreignTypeDeclarations manifest) (M.keys $ manifestTypes manifest)

instanceDeclarations :: Manifest -> Text -> [Text]
instanceDeclarations manifest key =
  let ty = lookupType manifest key
      rawName = capitalize . fromRaw (typeToHaskell manifest) $ key
      apiName = typeToHaskell manifest key
      constructorName = capitalize apiName
      dimStr = T.pack . show $ dim
      element = typeToHaskell manifest $ tyArrayElemType ty
      dim = tyArrayRank ty
  in  declareObject apiName constructorName rawName
      ++
      case ty of
        TypeArray  {} ->
            declareArray apiName constructorName rawName dimStr element
        TypeOpaque {} ->
            []

instanceDeclarationLines :: Manifest -> [Text]
instanceDeclarationLines manifest = concatMap (instanceDeclarations manifest) $ M.keys (manifestTypes manifest)

entryCallLines :: Manifest -> [Text]
entryCallLines manifest = concatMap (entryCall manifest) $ M.keys (manifestEntryPoints manifest)
