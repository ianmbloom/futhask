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

selectPtrLevel :: Int -> Int -> (FutharkType -> HsType') -> FutharkType -> HsType'
selectPtrLevel primLevel arrayLevel f ty =
  let level = if isPrim ty
              then primLevel
              else arrayLevel
  in  ptrs level (f ty)

arrayNewCall :: FutharkType -> HsExpr'
arrayNewCall ty =
  let haskName    = "new_" <> buildArrayName ty
      cName       = "futhark_" <> haskName
      arrayType   = ptr (var . up . futPrimToHask . tyElem $ ty)
      shapeParams = replicate (tyRank ty) (var "Int64")
      returnType  = ptr (var . typeRawName $ ty)
  in  foreignImportCall haskName cName (arrayType:shapeParams) returnType

arrayFreeCall :: FutharkType -> HsExpr'
arrayFreeCall ty =
  let haskName    = "free_" <> buildArrayName ty
      cName       = "futhark_" <> haskName
      inputType   = ptr (var . typeRawName $ ty)
      returnType  = var "Int"
  in  foreignImportCall haskName cName [inputType] returnType

arrayValuesCall :: FutharkType -> HsExpr'
arrayValuesCall ty =
  let haskName    = "values_" <> buildArrayName ty
      cName       = "futhark_" <> haskName
      inputType   = ptr (var . typeRawName $ ty)
      outputType  = ptr (var . up . futPrimToHask . tyElem $ ty)
      returnType  = var "Int"
  in  foreignImportCall haskName cName [inputType, outputType] returnType

arrayShapeCall :: FutharkType -> HsExpr'
arrayShapeCall ty =
  let haskName    = "shape_" <> buildArrayName ty
      cName       = "futhark_" <> haskName
      inputTypes  = ptr (var . typeRawName $ ty)
      returnType  = ptr (var "Int64")
  in  foreignImportCall haskName cName [inputTypes] returnType

opaqueFreeCall :: FutharkType -> HsExpr'
opaqueFreeCall ty =
  let haskName   = "free_" <> tyOpaqueBase ty
      cName      = "futhark_" <> "free_opaque_" <> tyOpaqueBase ty
      inputTypes = [ ptr (var . typeRawName $ ty)
                   ]
      returnType = (var "Int")
  in  foreignImportCall haskName cName inputTypes returnType

opaqueStoreCall :: FutharkType -> HsExpr'
opaqueStoreCall ty =
  let haskName    = "store_" <> tyOpaqueBase ty
      cName       = "futhark_" <> "store_opaque_" <> tyOpaqueBase ty
      inputTypes = [ ptr (var . typeRawName $ ty)
                   , ptrs 2 (var "()")
                   , ptr (var "CSize")
                   ]
      returnType = (var "Int")
  in  foreignImportCall haskName cName inputTypes returnType

opaqueRestoreCall :: FutharkType -> HsExpr'
opaqueRestoreCall ty =
  let haskName  = "restore_" <> tyOpaqueBase ty
      cName     = "futhark_" <> "restore_opaque_" <> tyOpaqueBase ty
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
        cEntryName = entryCCallName entry
        outputTypes = map (selectPtrLevel 1 2 (var . typeRawName) . pType ) $ futEntryOutParams entry
        inputTypes  = map (selectPtrLevel 0 1 (var . typeRawName) . pType ) $ futEntryInParams  entry
        params      = outputTypes <> inputTypes
    in  foreignImportCall entryName cEntryName params (var "Int")

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

foreignDataWrappers :: [FutharkType] -> [HsDecl']
foreignDataWrappers = map foreignDataDeclaration

foreignTypeOpDeclarations :: [FutharkType] -> [HsExpr']
foreignTypeOpDeclarations = concatMap foreignTypeOpDeclarationSet

foreignEntryDeclarations :: [FutharkEntry] -> [HsExpr']
foreignEntryDeclarations = map foreignEntryDeclaration

haskellDataWrappers :: [FutharkType] -> [HsDecl']
haskellDataWrappers = map dataDeclaration
haskellInstanceDeclarations :: [FutharkType] -> [HsDecl']
haskellInstanceDeclarations = concatMap instanceDeclaration

haskellEntryDeclarations :: [FutharkEntry] -> [HsDecl']
haskellEntryDeclarations entries = concatMap declareEntry entries
