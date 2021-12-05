{-# LANGUAGE OverloadedStrings #-}

module Generate

where

import Data.Maybe (fromJust)
import Data.Char (toUpper)
import qualified Data.Text as T
import Data.Text (Text(..))


import GHC.SourceGen

import qualified Data.Map as M
import Manifest

import Name
import Type

malloc             :: HsExpr'
withFO             :: HsExpr'
peekFree           :: HsExpr'
peekFreeWrapIn     :: HsExpr'
inContextWithError :: HsExpr'
unsafeLiftFromIO'  :: HsExpr'
monadConstraint    :: HsType'
malloc             = var (qual "F" "malloc")
withFO             = var (qual "T" "withFO")
peekFree           = var (qual "U" "peekFree"          )
peekFreeWrapIn     = var (qual "U" "peekFreeWrapIn"    ) @@ var "context"
inContextWithError = var (qual "C" "inContextWithError")
unsafeLiftFromIO'  = var (qual "Fut" "unsafeLiftFromIO")
monadConstraint    = var "Monad" @@ var "m"

futTMonad :: HsType'
futTMonad = var "FutT" @@ var "c" @@ var "m"
return' :: HsExpr'
return' = var "return"

append :: [a] -> a -> [a]
append xs y = xs ++ [y]

mightTuple :: HasTuple a => [a] -> a
mightTuple as =
  if length as > 1
  then tuple as
  else head as

ptr :: HsType' -> HsType'
ptr a = (bvar "Ptr") @@ a

ptrs :: Int -> HsType' -> HsType'
ptrs i a =
  if i > 0
  then ptr (ptrs (i-1) a)
  else a

foreignDataDeclaration :: FutharkType -> HsDecl'
foreignDataDeclaration ty =
    data' (typeRawName ty) [] [] []

dataDeclaration :: FutharkType -> HsDecl'
dataDeclaration ty =
   data' (constructorName $ ty) [bvar "c"]
         [prefixCon (constructorName $ ty)
            [ field (var (qual "MV" "MVar") @@ var "Int")
            , field (var (qual "F" "ForeignPtr") @@ var (qual "Raw" (typeRawName ty)))
            ]
         ] []


generateFFIImport :: String -> String -> [HsType'] -> HsType' -> HsExpr'
generateFFIImport haskCallName cCallName params ret =
    let ctx       = ptr (var "Futhark_context")
    in
       (    var "foreign"
         @@ var "import"
         @@ var "ccall"
         @@ var "unsafe"
         @@ string ("futhark_" <> cCallName)
         @@ (var . up $ haskCallName)
      )
      @::@
      foldr1 (-->) (ctx:params <> [var "IO" @@ ret])

declareObject :: FutharkType -> HsDecl'
declareObject ty =
  let con         = up $ constructorName ty
      qualRaw     = up $ "Raw." <> typeRawName ty
      qualRawFree = up $ "Raw.free_" <> typeApiName ty
  in
    instance' (var "FutharkObject" @@ var con @@ var qualRaw)
      [ funBind "wrapFO" $ match [] $ var con
      , funBind "freeFO" $ match [] $ var qualRawFree
      , funBind "fromFO" $ match [conP con [bvar "rc", bvar "fp"]] $ tuple [var "rc", var "fp"]
      ]

declareArray :: FutharkType -> HsDecl'
declareArray ty =
  let con           = var            $ constructorName ty
      qualRaw       = var . qual raw $ typeRawName     ty
      qualRawShape  = var . qual raw $ shapeApiName    ty
      qualRawNew    = var . qual raw $ newApiName      ty
      qualRawValues = var . qual raw $ valuesApiName   ty
      elt           = var . up $ futPrimToHask . tyElem $ ty
      dim           = tyRank ty
      dimVar        = var . up $ "M.Ix" <> show dim
      toFun         = var . up $ "to"   <> show dim <> "d"
      fromFun       = var . up $ "from" <> show dim <> "d"
  in  instance' (var "FutharkArray" @@ con @@ qualRaw @@ dimVar @@ elt)
        [ funBind "shapeFA"  $ match [] $ toFun   @@ qualRawShape
        , funBind "newFA"    $ match [] $ fromFun @@ qualRawNew
        , funBind "valuesFA" $ match [] $ qualRawValues
        ]

type LayerOp = [PName] -> [PName] -> ([Stmt'],[Stmt'])

foldLayers :: [a -> a] -> a -> a
foldLayers layers body = foldr ($) body layers

declareEntry :: FutharkEntry -> [HsDecl']
declareEntry entry =
  [ typeDeclaration
  , funcDeclaration
  ]
  where
    inParams  :: [FutharkParameter]
    inParams  = futEntryInParams  entry
    outParams :: [FutharkParameter]
    outParams = futEntryOutParams entry
    entryName :: OccNameStr
    entryName = entryApiName entry
    outType :: FutharkParameter -> HsType'
    outType ty = (mightApplySkolem constructorName . pType $ ty)
    (call, postCall) = entryParts entry
    typeDeclaration :: HsDecl'
    typeDeclaration =   typeSig entryName
                    $   [monadConstraint]
                    ==> foldr1 (-->) (map outType inParams ++
                        [futTMonad @@ mightTuple (map outType outParams)])
    funcDeclaration :: HsDecl'
    funcDeclaration = funBind entryName . match (map (bvar . up . pName) inParams) $
                          op unsafeLiftFromIO' "$" $
                          lambda [bvar "context"] $
                          withFOLayers inParams $
                          (do' (call ++ postCall))

entryParts :: FutharkEntry -> ([Stmt'], [Stmt'])
entryParts entry = appliedLayers [] []
  where
    inParams  :: [FutharkParameter]
    inParams  = futEntryInParams  entry
    outParams :: [FutharkParameter]
    outParams = futEntryOutParams entry
    applyEntry :: [PName] -> [PName] -> Stmt'
    applyEntry ins outs = stmt $
                     inContextWithError @@ var "context" @@ lambda [bvar . up . prime $ "context"]
                     (foldl1 (@@) $
                         [var (entryQualRawName entry), var . up . prime $ "context"]
                         ++ map (var . up) outs
                         ++ map (var . up) ins
                         )
    returnOut :: [PName] -> Stmt'
    returnOut outs = stmt $
                     return' @@ (mightTuple $ (map (bvar . up . prime) outs :: [HsExpr']))
    coreBodies :: LayerOp
    coreBodies ins outs = ([applyEntry ins outs], [returnOut outs])
    inLayers :: [LayerOp -> LayerOp]
    inLayers  = map layerIn inParams
    outLayers :: [LayerOp -> LayerOp]
    outLayers = map layerOut outParams
    appliedLayers :: LayerOp
    appliedLayers = foldLayers inLayers $ foldLayers outLayers coreBodies

    layerIn :: FutharkParameter -> LayerOp -> LayerOp
    layerIn param =
      if isPrim (pType param)
      then layerPrimitive  (pName param)
      else layerForeignPtr (pName param)
    layerPrimitive :: PName -> LayerOp -> LayerOp
    layerPrimitive name body inVars outVars =
        body (append inVars name) outVars
    layerForeignPtr :: PName -> LayerOp -> LayerOp
    layerForeignPtr name body inVars outVars =
        body (append inVars $ prime name) outVars
    layerOut :: FutharkParameter -> LayerOp -> LayerOp
    layerOut param =
      let wrapIn = not . isPrim . pType $ param
      in  layerMalloc wrapIn (pName param)
    layerMalloc :: Bool -> PName -> LayerOp -> LayerOp
    layerMalloc wrapIn name body inVars outVars =
        let (retInBody, retOutBody)  = body inVars (append outVars name)
            peekF = if wrapIn then peekFreeWrapIn else peekFree
        in  ( ((bvar . up         $ name) <-- malloc                    ):retInBody
            , ((bvar . up . prime $ name) <-- peekF @@ (var . up $ name)):retOutBody
            )

withFOLayers :: [FutharkParameter] -> HsExpr' -> HsExpr'
withFOLayers inParams expr =
   foldr ($) expr fOLayers
   where
      lambdaWithFO :: FutharkParameter -> HsExpr' -> HsExpr'
      lambdaWithFO param body =
        if isPrim . pType $ param
        then body
        else op (withFO @@ (var . up $ pName param)) "$" (lambda [bvar . up . prime . pName $ param] body)
      fOLayers :: [HsExpr'->HsExpr']
      fOLayers = map lambdaWithFO inParams
