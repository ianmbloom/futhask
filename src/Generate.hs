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
malloc             = var $ qual "F" "malloc"
withFO             = var (qual "T" "withFO")
peekFree           = var (qual "U" "peekFree"          )
peekFreeWrapIn     = var (qual "U" "peekFreeWrapIn"    ) @@ var "context"
inContextWithError = var (qual "C" "inContextWithError")
futT               :: HsType'
c                  :: HsType'
m                  :: HsType'
futT               = var "FutT"
c                  = var "c"
m                  = var "m"
return' :: HsExpr'
return'            = var "return"

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


foreignImportCall :: String -> String -> [HsType'] -> HsType' -> HsExpr'
foreignImportCall haskCallName cCallName params ret =
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

declareObject :: String -> String -> String -> HsDecl'
declareObject apiName constructorName rawName =
  let con         = up $ constructorName
      qualRaw     = up $ "Raw." <> rawName
      qualRawFree = up $ "Raw.free_" <> apiName
  in
    instance' (var "FutharkObject" @@ var con @@ var qualRaw)
      [ funBind "wrapFO" $ match [] $ var con
      , funBind "freeFO" $ match [] $ var qualRawFree
      , funBind "fromFO" $ match [conP con [bvar "rc", bvar "fp"]] $ tuple [var "rc", var "fp"]
      ]

declareArray :: String -> String -> String -> Int -> String -> HsDecl'
declareArray apiName constructorName rawName dim element =
  let con           = var .            up $ constructorName
      elt           = var .            up $ element
      qualRaw       = var . qual raw . up $             rawName
      qualRawShape  = var . qual raw . up $ "shape_" <> apiName
      qualRawNew    = var . qual raw . up $ "new_"   <> apiName
      qualRawValues = var . qual raw . up $ "values_"<> apiName
      dimVar        = var .            up $ "M.Ix"   <> show dim
      toFun         = var .            up $ "to"     <> show dim <> "d"
      fromFun       = var .            up $ "from"   <> show dim <> "d"
  in  instance' (var "FutharkArray" @@ con @@ qualRaw @@ dimVar @@ elt)
        [ funBind "shapeFA"  $ match [] $ toFun   @@ qualRawShape
        , funBind "newFA"    $ match [] $ fromFun @@ qualRawNew
        , funBind "valuesFA" $ match [] $ qualRawValues
        ]

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
    lambdaWithFO :: FutharkParameter -> HsExpr' -> HsExpr'
    lambdaWithFO param body =
      if isPrim . pType $ param
      then body
      else op (withFO @@ (var . up $ pName param)) "$" (lambda [bvar . up . prime . pName $ param] body)
    fOLayers = map lambdaWithFO inParams
    inLayers :: [([PName] -> [PName] -> ([Stmt'],[Stmt'])) ->  [PName] -> [PName] -> ([Stmt'],[Stmt'])]
    inLayers  = map layerIn inParams
    outLayers :: [([PName] -> [PName] -> ([Stmt'],[Stmt'])) ->  [PName] -> [PName] -> ([Stmt'],[Stmt'])]
    outLayers = map layerOut outParams
    layers :: [PName] -> [PName] -> ([Stmt'],[Stmt'])
    layers = foldl (flip ($)) (foldl (flip ($)) body outLayers) inLayers
    (call, postCall) = layers [] []
    entryName :: OccNameStr
    entryName = entryApiName entry
    outType :: FutharkParameter -> HsType'
    outType ty = (mightApplySkolem constructorName . pType $ ty)
    typeDeclaration :: HsDecl'
    typeDeclaration =   typeSig entryName
                    $   [var "Monad" @@ var "m"]
                    ==> foldr1 (-->) (map outType inParams ++
                        [futT @@ c @@ m @@ mightTuple (map outType outParams)])
    funcDeclaration :: HsDecl'
    funcDeclaration = funBind entryName . match (map (bvar . up . pName) inParams) $
                          op (var $ qual "Fut" "unsafeLiftFromIO") "$" $
                          lambda [bvar "context"] $
                          foldr ($)
                          (do' (call ++ postCall)) fOLayers
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
    body :: [PName] -> [PName] -> ([Stmt'], [Stmt'])
    body ins outs = ([applyEntry ins outs], [returnOut outs])
    layerIn :: FutharkParameter
            -> ([PName] -> [PName] -> ([Stmt'],[Stmt']))
            ->  [PName] -> [PName] -> ([Stmt'],[Stmt'])
    layerIn param =
      if isPrim (pType param)
      then layerPrimitive  (pName param)
      else layerForeignPtr (pName param)
    layerPrimitive :: PName
                   -> ([PName] -> [PName] -> ([Stmt'],[Stmt']))
                   ->  [PName] -> [PName] -> ([Stmt'],[Stmt'])
    layerPrimitive name body inVars outVars =
        body (name:inVars) outVars
    layerForeignPtr :: PName
                    -> ([PName] -> [PName] -> ([Stmt'],[Stmt']))
                    ->  [PName] -> [PName] -> ([Stmt'],[Stmt'])
    layerForeignPtr name body inVars outVars =
        let ( retInBody, retOutBody) = body (prime name:inVars) outVars
        in  ( retInBody
            , retOutBody
            )
    layerOut :: FutharkParameter
            -> ([PName] -> [PName] -> ([Stmt'],[Stmt']))
            ->  [PName] -> [PName] -> ([Stmt'],[Stmt'])
    layerOut param =
      let wrapIn = not . isPrim . pType $ param
      in  layerMalloc wrapIn (pName param)

    layerMalloc :: Bool
                -> PName
                -> ([PName] -> [PName] -> ([Stmt'],[Stmt']))
                ->  [PName] -> [PName] -> ([Stmt'],[Stmt'])
    layerMalloc wrapIn name body inVars outVars =
        let (retInBody, retOutBody)  = body inVars (name:outVars)
            peekF = if wrapIn then peekFreeWrapIn else peekFree
        in  ( [(bvar . up         $ name) <-- malloc] ++ retInBody
            , [(bvar . up . prime $ name) <-- peekF @@ (var . up $ name)] ++ retOutBody
            )
