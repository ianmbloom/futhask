{-# LANGUAGE OverloadedStrings #-}

module Declarations where

import Data.Maybe
import Data.List (intercalate, partition, lookup)
import Data.Char (toUpper)
import Text.ParserCombinators.ReadP
import qualified Data.Map  as M
import Data.Map ((!))
import Debug
import Data.String (IsString(..))
import Control.Monad
import GHC.SourceGen

import Generate
import Name
import Type

arrayNewCall :: FutharkType -> HsExpr'
arrayNewCall ty =
  let haskName    = newApiName ty
      cName       = newCName   ty
      arrayType   = ptr (var . up . futPrimToHask . tyElem $ ty)
      shapeParams = replicate (tyRank ty) (var "Int64")
      returnType  = ptr (var . typeRawName $ ty)
  in  generateFFIImport haskName cName (arrayType:shapeParams) returnType

arrayFreeCall :: FutharkType -> HsExpr'
arrayFreeCall ty =
  let haskName    = freeApiName ty
      cName       = freeCName   ty
      inputType   = ptr (var . typeRawName $ ty)
      returnType  = var "Int"
  in  generateFFIImport haskName cName [inputType] returnType

arrayValuesCall :: FutharkType -> HsExpr'
arrayValuesCall ty =
  let haskName    = valuesApiName ty
      cName       = valuesCName   ty
      inputType   = ptr (var . typeRawName $ ty)
      outputType  = ptr (var . up . futPrimToHask . tyElem $ ty)
      returnType  = var "Int"
  in  generateFFIImport haskName cName [inputType, outputType] returnType

arrayShapeCall :: FutharkType -> HsExpr'
arrayShapeCall ty =
  let haskName    = shapeApiName ty
      cName       = shapeCName   ty
      inputTypes  = ptr (var . typeRawName $ ty)
      returnType  = ptr (var "Int64")
  in  generateFFIImport haskName cName [inputTypes] returnType

opaqueFreeCall :: FutharkType -> HsExpr'
opaqueFreeCall ty =
  let haskName   = freeApiName ty
      cName      = freeCName   ty
      inputTypes = [ ptr (var . typeRawName $ ty)
                   ]
      returnType = (var "Int")
  in  generateFFIImport haskName cName inputTypes returnType

opaqueStoreCall :: FutharkType -> HsExpr'
opaqueStoreCall ty =
  let haskName    = storeApiName ty
      cName       = storeCName   ty
      inputTypes = [ ptr (var . typeRawName $ ty)
                   , ptrs 2 (var "()")
                   , ptr (var "CSize")
                   ]
      returnType = (var "Int")
  in  generateFFIImport haskName cName inputTypes returnType

opaqueRestoreCall :: FutharkType -> HsExpr'
opaqueRestoreCall ty =
  let haskName   = restoreApiName ty
      cName      = restoreCName   ty
      inputTypes = [ptr (var "()")]
      returnType = ptr (var . typeRawName $ ty)
  in  generateFFIImport haskName cName inputTypes returnType

foreignTypeOpDeclarationSet :: FutharkType -> [HsExpr']
foreignTypeOpDeclarationSet ty =
    case ty of
      Array {} ->
        [ arrayNewCall    ty
        , arrayFreeCall   ty
        , arrayValuesCall ty
        , arrayShapeCall  ty
        ]
      Opaque {} ->
        [ opaqueFreeCall    ty
        , opaqueStoreCall   ty
        , opaqueRestoreCall ty
        ]

addPtrToNonPrim :: (FutharkType -> HsType') -> FutharkType -> HsType'
addPtrToNonPrim f ty =
  if isPrim ty
  then      f ty
  else ptr (f ty)

foreignEntryDeclaration :: FutharkEntry -> HsExpr'
foreignEntryDeclaration entry =
    let entryName  = entryRawName entry
        outputTypes = map (ptr . addPtrToNonPrim (var . typeRawName) . pType ) $ futEntryOutParams entry
        inputTypes  = map (      addPtrToNonPrim (var . typeRawName) . pType ) $ futEntryInParams  entry
        params      = outputTypes <> inputTypes
    in  generateFFIImport entryName entryName params (var "Int")

instanceDeclaration :: FutharkType -> [HsDecl']
instanceDeclaration ty =
      declareObject ty:if isArray ty then [declareArray ty] else []
