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

primTable =
    [ ("f32", "Float" )
    , ("f64", "Double")
    , ("bool","CBool" )
    , ("i8" , "Int8"  )
    , ("i16", "Int16" )
    , ("i32", "Int32" )
    , ("i64", "Int64" )
    , ("u8" , "Word8" )
    , ("u16", "Word16")
    , ("u32", "Word32")
    , ("u64", "Word64") ]

capitalize :: Text -> Text
capitalize text = T.singleton (toUpper (T.head text)) <> T.tail text
wrapIfNotOneWord :: Text -> Text
wrapIfNotOneWord s = if length (T.words s) > 1 then "(" <> s <> ")" else s

dropText :: Text -> Text -> Text
dropText text n =
  let len = T.length text
      check = T.take len n
  in  if check == text
      then T.drop len n
      else error $ "dropText: " ++ T.unpack n ++ "does not start with " ++ T.unpack text ++ "."

dropFuthark     = dropText "futhark_"

mapTail g f (a:as) = g a:map f as
mapTail g f [] = []

pointerLevel :: Int -> Text -> Text
pointerLevel i a =
  if i > 0
  then "Ptr " <> wrapIfNotOneWord (pointerLevel (i-1) a)
  else a

data WithSkolem = WithSkolem | NoSkolem
data NameSource = Raw | Api

futharkPrimitiveToHaskellType :: Text -> Text
futharkPrimitiveToHaskellType primitive =
  case lookup primitive primTable of
    Just  s -> s
    Nothing -> error $ "type \'" ++ T.unpack primitive ++ "\' not found."

buildArrayType :: Type -> Text
buildArrayType ty =
  let elemType = tyArrayElemType ty
      rank     = tyArrayRank     ty
  in  elemType <> "_" <> (T.pack $ show rank) <> "d"

typeToHaskellPointer :: Manifest -> WithSkolem -> NameSource -> Int -> Int -> Text -> Text
typeToHaskellPointer manifest skolem src primLevel arrayLevel key =
  let ty       = manifestTypes manifest ! key
      mPrim    = lookup key primTable
      level    = case mPrim of
                   Just {} -> primLevel
                   Nothing -> arrayLevel
      prefix   = case src of
                   Raw -> "futark_"
                   Api -> ""
      suffix   = case skolem of
                   NoSkolem   -> ""
                   WithSkolem -> " c"
  in pointerLevel level $
     case mPrim of
        Just prim -> prim
        Nothing   ->
          case ty of
            TypeArray  {} -> prefix <> buildArrayType ty <> suffix
            TypeOpaque {} -> prefix <> "opaque_" <> key  <> suffix

typeToHaskell :: Manifest -> WithSkolem -> NameSource -> Text -> Text
typeToHaskell manifest skol src key =
  typeToHaskellPointer manifest skol src 0 0 key

foreignDataDeclaration :: Text -> [Text]
foreignDataDeclaration haskellTy =
    ["data " <> capitalize haskellTy ]

foreignImportCall :: Text -> [Text] -> Text -> [Text]
foreignImportCall arrName params ret =
    let ctx       = pointerLevel 1 "Futhark_context"
    in
    [ "foreign import ccall unsafe \"futhark_" <> arrName <> "\""
    , "  " <> arrName
    ]
    ++ indentTail "    :: " "    -> "
       (ctx:params <> ["IO " <> wrapIfNotOneWord ret])

primPointer = pointerLevel 1 . futharkPrimitiveToHaskellType . tyArrayElemType

arrayNewCall :: Text -> Type -> [Text]
arrayNewCall haskellTy ty =
  let arrName     = "new_" <> buildArrayType ty
      arrayType   = primPointer ty
      shapeParams = replicate (tyArrayRank ty) "Int64"
      returnType  = pointerLevel 1 haskellTy
  in  foreignImportCall arrName (arrayType:shapeParams) returnType

arrayFreeCall :: Text -> Type -> [Text]
arrayFreeCall haskellTy ty =
  let arrName     = "free_" <> buildArrayType ty
      inputType   = pointerLevel 1 haskellTy
      returnType  = "Int"
  in  foreignImportCall arrName [inputType] returnType

arrayValuesCall :: Text -> Type -> [Text]
arrayValuesCall haskellTy ty =
  let arrName     = "values_" <> buildArrayType ty
      inputType   = pointerLevel 1 haskellTy
      outputType  = primPointer ty
      returnType  = "Int"
  in  foreignImportCall arrName [inputType, outputType] returnType

arrayShapeCall :: Text -> Type -> [Text]
arrayShapeCall haskellTy ty =
  let arrName     = "shape_" <> buildArrayType ty
      inputTypes  = pointerLevel 1 haskellTy
      returnType  = pointerLevel 1 "Int64"
  in  foreignImportCall arrName [inputTypes] returnType

opaqueFreeCall :: Text -> Type -> [Text]
opaqueFreeCall haskellTy ty =
  let arrName    = dropFuthark . opaqueFree . tyOpaqueOps $ ty
      inputTypes = [ pointerLevel 1 haskellTy
                   ]
      returnType = "Int"
  in  foreignImportCall arrName inputTypes returnType

opaqueStoreCall :: Text -> Type -> [Text]
opaqueStoreCall haskellTy ty =
  let arrName    = dropFuthark . opaqueStore . tyOpaqueOps $ ty
      inputTypes = [ pointerLevel 1 haskellTy
                   , pointerLevel 2 "()"
                   , pointerLevel 1 "CSize"
                   ]
      returnType = "Int"
  in  foreignImportCall arrName inputTypes returnType

opaqueRestoreCall :: Text -> Type -> [Text]
opaqueRestoreCall haskellTy ty =
  let arrName    = dropFuthark . opaqueRestore . tyOpaqueOps $ ty
      inputTypes = [pointerLevel 1 "()"]
      returnType = pointerLevel 1 haskellTy
  in  foreignImportCall arrName inputTypes returnType

foreignTypeDeclarations :: Manifest -> Text -> Type -> [Text]
foreignTypeDeclarations manifest key ty =
    let haskellTy = typeToHaskell manifest NoSkolem Raw key
    in
    foreignDataDeclaration haskellTy
    <>
    case ty of
      TypeArray {} ->
           arrayNewCall      haskellTy ty
        <> arrayFreeCall     haskellTy ty
        <> arrayValuesCall   haskellTy ty
        <> arrayShapeCall    haskellTy ty
      TypeOpaque {} ->
           opaqueFreeCall    haskellTy ty
        <> opaqueStoreCall   haskellTy ty
        <> opaqueRestoreCall haskellTy ty

foreignEntryDeclaration :: Manifest -> Text -> EntryPoint -> [Text]
foreignEntryDeclaration manifest _futEntryName entry =
    let arrName     = dropFuthark $ entryPointCFun entry
        outputTypes = map (capitalize . typeToHaskellPointer manifest NoSkolem Raw 1 2 . outputType) $ entryPointOutputs entry
        inputTypes  = map (capitalize . typeToHaskellPointer manifest NoSkolem Raw 0 1 . inputType ) $ entryPointInputs entry
        params      = outputTypes <> inputTypes
    in  foreignImportCall arrName params "Int"

rawImportString :: Manifest -> [Text]
rawImportString manifest =
  concatMap (uncurry (foreignEntryDeclaration manifest)) (M.assocs $ manifestEntryPoints manifest)
  ++
  concatMap (uncurry (foreignTypeDeclarations manifest)) (M.assocs $ manifestTypes manifest)

instanceDeclarations :: Manifest -> Text -> Type -> [Text]
instanceDeclarations manifest key ty =
  let rawName = capitalize $ typeToHaskell manifest NoSkolem Raw key
      apiName = typeToHaskell manifest NoSkolem Api key
      constructorName = capitalize apiName
      dimStr = T.pack . show $ dim
      element = futharkPrimitiveToHaskellType $ tyArrayElemType ty
      dim = tyArrayRank ty
  in  declareObject apiName constructorName rawName
      ++
      case ty of
        TypeArray  {} ->
            declareArray apiName constructorName rawName dimStr element
        TypeOpaque {} ->
            []

declareObject :: Text -> Text -> Text -> [Text]
declareObject apiName constructorName rawName =
  [ "data " <> constructorName <> " c = " <> constructorName <> " (MV.MVar Int) (F.ForeignPtr Raw." <> rawName <> ")"
  , "instance FutharkObject " <> constructorName <> " Raw." <> rawName <> " where"
  , "  wrapFO = " <> constructorName
  , "  freeFO = Raw.free_" <> apiName
  , "  fromFO (" <> constructorName <> " rc fp) = (rc, fp)"
  ]

declareArray :: Text -> Text -> Text -> Text -> Text -> [Text]
declareArray apiName constructorName rawName dimStr element =
  [ "instance FutharkArray " <> constructorName <> " Raw." <> rawName <> " M.Ix" <> dimStr <> " " <> element <> " where"
  , "  shapeFA  = to" <> dimStr <> "d Raw.shape_" <> apiName
  , "  newFA    = from" <> dimStr <> "d Raw.new_" <> apiName
  , "  valuesFA = Raw.values_" <> apiName
  ]

instanceDeclarationLines :: Manifest -> [Text]
instanceDeclarationLines manifest = concatMap (uncurry (instanceDeclarations manifest)) $ M.assocs (manifestTypes manifest)

indentTail a b = mapTail (a<>) (b<>)

inTupleMore ls =
  if length ls > 1
  then indentTail "( " ", " ls <> [")"]
  else ls

makeConstraints :: [Text] -> [Text]
makeConstraints = inTupleMore

typeWithConstraints :: [Text] -> [Text] -> [Text]
typeWithConstraints constraintLines typeLines =
  if length constraintLines > 0
  then    indentTail " :: " "    " constraintLines
       <> indentTail " => " " -> " typeLines
  else    indentTail " :: " " -> " typeLines

mightTuple :: [Text] -> Text
mightTuple ls =
  if length ls <= 1
  then wrapIfNotOneWord $ T.concat ls
  else "(" <> T.intercalate ", " ls <> ")"

arrowBefore :: Text -> Text
arrowBefore x = "  -> " <> x

entryCall :: Manifest -> Text -> EntryPoint -> [Text]
entryCall manifest entryName entryPoint =
         typeDeclaration
      ++ input
      ++ map arrowBefore (   preCall
                          ++ call
                         )
      ++ postCall
      ++ [""]
    where
        apiName = "entry_" <> entryName
        isForeignI a = isJust $ M.lookup (inputType  a) (manifestTypes manifest)
        isForeignO a = isJust $ M.lookup (outputType a) (manifestTypes manifest)
        inArgs   = entryPointInputs  entryPoint
        outArgs  = entryPointOutputs entryPoint
        inArgTys    = map inputType  inArgs
        inArgNames  = map inputName  inArgs
        outArgTys   = map outputType outArgs
        outArgNames = map (("out"<>) . T.pack . show) [0..length outArgTys-1]
        inputHaskTypes  = map (capitalize . typeToHaskellPointer manifest WithSkolem Api 0 0 . inputType ) inArgs
        outputHaskTypes = map (capitalize . typeToHaskellPointer manifest WithSkolem Api 0 0 . outputType) outArgs
        foreignInputs = filter isForeignI inArgs
        typeDeclaration = [entryName] ++
                          (typeWithConstraints ["Monad m"] $
                              inputHaskTypes
                              ++ ["FutT c m " <> mightTuple outputHaskTypes
                                 ]
                          )
        input = [ T.unwords (entryName : map inputName inArgs)
                , "  =  Fut.unsafeLiftFromIO $ \\context"
                ]
        preCall =  (map (\i -> "T.withFO " <> i <> " $ \\" <> i <> "'") (map inputName foreignInputs))
                ++ (map (\o -> "F.malloc >>= \\" <> o) outArgNames)
        call = [ "C.inContextWithError context (\\context'"
               , "Raw." <> apiName <> " context' " <> T.unwords (outArgNames <> (map (\i -> inputName i <> if isForeignI i then "'" else "") inArgs)) <> ")"
               ]
        peek o = if isForeignO o then "U.peekFreeWrapIn context " else "U.peekFree "
        postCall = indentTail "  >> " "  -> " $
                   if length outArgs > 1
                   then  zipWith (\o name -> peek o <> name <> " >>= \\" <> name <> "'") outArgs outArgNames
                         ++ ["return " <> mightTuple (map (\o -> o <> "'") outArgNames)]
                   else [peek (head outArgs) <> head outArgNames]

entryCallLines :: Manifest -> [Text]
entryCallLines manifest = concat . map (uncurry (entryCall manifest)) $ M.assocs . manifestEntryPoints $ manifest


{-

startsWith parts n = take (length parts) n == parts

isStruct :: [Text] -> Bool
isStruct = startsWith ["struct"]


dropEntry       = dropText "entry_"
fromPointer     = T.dropWhile (/='*')
beforePointer   = T.takeWhile (/='*')
withoutConst    = dropWhile (=="const")
withoutUnsigned = dropWhile (=="unsigned")

  let ts = withoutUnsigned $ withoutConst $ T.words $ simple
  in  if isStruct ts
      then let part = ts !! 1
               cropPart = case src of
                             Raw -> part
                             Api -> dropFuthark part
           in  cropPart
      else futharkPrimitiveToHaskellType (head ts)

lookupType :: Manifest -> T.Text -> Either Type T.Text
lookupType manifest text =
  case manifestTypes manifest !? text of
    Nothing -> Right text
    Just ty -> Left  ty

foreignTypeToHaskell :: Manifest -> NameSource -> Text -> Type -> Text
foreignTypeToHaskell manifest src key ty =


-}
