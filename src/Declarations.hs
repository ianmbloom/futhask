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
import GHC.SourceGen

import Generate
import Name
import Type

addPtrToArray :: (FutharkType -> HsType') -> FutharkType -> HsType'
addPtrToArray f ty =
  let level = if isPrim ty
              then id
              else ptr
  in  level (f ty)

arrayNewCall :: FutharkType -> HsExpr'
arrayNewCall ty =
  let haskName    = "new_" <> buildArrayName ty
      cName       = haskName
      arrayType   = ptr (var . up . futPrimToHask . tyElem $ ty)
      shapeParams = replicate (tyRank ty) (var "Int64")
      returnType  = ptr (var . typeRawName $ ty)
  in  foreignImportCall haskName cName (arrayType:shapeParams) returnType

arrayFreeCall :: FutharkType -> HsExpr'
arrayFreeCall ty =
  let haskName    = "free_" <> buildArrayName ty
      cName       = haskName
      inputType   = ptr (var . typeRawName $ ty)
      returnType  = var "Int"
  in  foreignImportCall haskName cName [inputType] returnType

arrayValuesCall :: FutharkType -> HsExpr'
arrayValuesCall ty =
  let haskName    = "values_" <> buildArrayName ty
      cName       = haskName
      inputType   = ptr (var . typeRawName $ ty)
      outputType  = ptr (var . up . futPrimToHask . tyElem $ ty)
      returnType  = var "Int"
  in  foreignImportCall haskName cName [inputType, outputType] returnType

arrayShapeCall :: FutharkType -> HsExpr'
arrayShapeCall ty =
  let haskName    = "shape_" <> buildArrayName ty
      cName       = haskName
      inputTypes  = ptr (var . typeRawName $ ty)
      returnType  = ptr (var "Int64")
  in  foreignImportCall haskName cName [inputTypes] returnType

opaqueFreeCall :: FutharkType -> HsExpr'
opaqueFreeCall ty =
  let haskName   = "free_" <> tyOpaqueBase ty
      cName      = "free_opaque_" <> tyOpaqueBase ty
      inputTypes = [ ptr (var . typeRawName $ ty)
                   ]
      returnType = (var "Int")
  in  foreignImportCall haskName cName inputTypes returnType

opaqueStoreCall :: FutharkType -> HsExpr'
opaqueStoreCall ty =
  let haskName    = "store_" <> tyOpaqueBase ty
      cName       = "store_opaque_" <> tyOpaqueBase ty
      inputTypes = [ ptr (var . typeRawName $ ty)
                   , ptrs 2 (var "()")
                   , ptr (var "CSize")
                   ]
      returnType = (var "Int")
  in  foreignImportCall haskName cName inputTypes returnType

opaqueRestoreCall :: FutharkType -> HsExpr'
opaqueRestoreCall ty =
  let haskName  = "restore_" <> tyOpaqueBase ty
      cName     = "restore_opaque_" <> tyOpaqueBase ty
      inputTypes = [ptr (var "()")]
      returnType = ptr (var . typeRawName $ ty)
  in  foreignImportCall haskName cName inputTypes returnType

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

foreignEntryDeclaration :: FutharkEntry -> HsExpr'
foreignEntryDeclaration entry =
    let entryName  = entryRawName entry
        outputTypes = map (ptr . addPtrToArray (var . typeRawName) . pType ) $ futEntryOutParams entry
        inputTypes  = map (      addPtrToArray (var . typeRawName) . pType ) $ futEntryInParams  entry
        params      = outputTypes <> inputTypes
    in  foreignImportCall entryName entryName params (var "Int")

instanceDeclaration :: FutharkType -> [HsDecl']
instanceDeclaration ty =
  let
      element = futPrimToHask . tyElem $ ty
      dim     = tyRank ty
  in  [declareObject (typeApiName ty) (constructorName ty ) (typeRawName ty)]
      ++
      case ty of
        Array  {} ->
            [declareArray (typeApiName ty) (constructorName ty) (typeRawName ty) dim element]
        Opaque {} ->
            []
