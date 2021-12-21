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

arrayCallNew :: FutharkType -> HsExpr'
arrayCallNew ty =
  let haskName    = apiNameNew ty
      cName       = cNameNew   ty
      arrayType   = ptr (var . up . futPrimToHask . tyElem $ ty)
      shapeParams = replicate (tyRank ty) (var "Int64")
      returnType  = ptr (var . typeRawName $ ty)
  in  generateFFIImport haskName cName (arrayType:shapeParams) returnType

arrayCallFree :: FutharkType -> HsExpr'
arrayCallFree ty =
  let haskName    = apiNameFree ty
      cName       = cNameFree   ty
      inputType   = ptr (var . typeRawName $ ty)
      returnType  = var "Int"
  in  generateFFIImport haskName cName [inputType] returnType

arrayCallValues :: FutharkType -> HsExpr'
arrayCallValues ty =
  let haskName    = apiNameValues ty
      cName       = cNameValues   ty
      inputType   = ptr (var . typeRawName $ ty)
      outputType  = ptr (var . up . futPrimToHask . tyElem $ ty)
      returnType  = var "Int"
  in  generateFFIImport haskName cName [inputType, outputType] returnType

arrayCallShape :: FutharkType -> HsExpr'
arrayCallShape ty =
  let haskName    = apiNameShape ty
      cName       = cNameShape   ty
      inputTypes  = ptr (var . typeRawName $ ty)
      returnType  = ptr (var "Int64")
  in  generateFFIImport haskName cName [inputTypes] returnType

opaqueCallFree :: FutharkType -> HsExpr'
opaqueCallFree ty =
  let haskName   = apiNameFree ty
      cName      = cNameFree   ty
      inputTypes = [ ptr (var . typeRawName $ ty)
                   ]
      returnType = (var "Int")
  in  generateFFIImport haskName cName inputTypes returnType

opaqueCallStore :: FutharkType -> HsExpr'
opaqueCallStore ty =
  let haskName    = apiNameStore ty
      cName       = cNameStore   ty
      inputTypes = [ ptr (var . typeRawName $ ty)
                   , ptrs 2 (var "()")
                   , ptr (var "CSize")
                   ]
      returnType = (var "Int")
  in  generateFFIImport haskName cName inputTypes returnType

opaqueCallRestore :: FutharkType -> HsExpr'
opaqueCallRestore ty =
  let haskName   = apiNameRestore ty
      cName      = cNameRestore   ty
      inputTypes = [ptr (var "()")]
      returnType = ptr (var . typeRawName $ ty)
  in  generateFFIImport haskName cName inputTypes returnType

foreignTypeOpDeclarationSet :: FutharkType -> [HsExpr']
foreignTypeOpDeclarationSet ty =
    case ty of
      Array {} ->
        [ arrayCallNew    ty
        , arrayCallFree   ty
        , arrayCallValues ty
        , arrayCallShape  ty
        ]
      Opaque {} ->
        [ opaqueCallFree    ty
        , opaqueCallStore   ty
        , opaqueCallRestore ty
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
