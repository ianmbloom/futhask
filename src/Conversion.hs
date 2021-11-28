{-# LANGUAGE OverloadedStrings #-}

module Conversion where

import Data.Maybe
import Data.List (intercalate, partition, lookup)
import Data.Char (toUpper)
import Text.ParserCombinators.ReadP
import ReadHeader
import Manifest
import qualified Data.Map  as M
import Data.Map ((!?))
import qualified Data.Text as T
import Data.Text (Text(..))
import Debug
import Data.String (IsString(..))

varTable =
    [ ("int", "Int")
    , ("float", "Float")
    , ("double", "Double")
    , ("char", "CChar")
    , ("bool", "CBool")
    , ("void", "()")
    , ("int8_t" , "Int8")
    , ("int16_t", "Int16")
    , ("int32_t", "Int32")
    , ("int64_t", "Int64")
    , ("uint8_t" , "Word8")
    , ("uint16_t", "Word16")
    , ("uint32_t", "Word32")
    , ("uint64_t", "Word64")
    , ("size_t", "CSize")
    , ("FILE", "CFile")
    , ("cl_mem", "CLMem")
    , ("cl_command_queue", "CLCommandQueue")
    , ("CUdeviceptr", "DevicePtr ()") ]

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

capitalize text = T.singleton (toUpper (T.head text)) <> T.tail text
wrapIfNotOneWord :: Text -> Text
wrapIfNotOneWord s = if length (T.words s) > 1 then "(" <> s <> ")" else s

startsWith parts n = take (length parts) n == parts

dropText :: Text -> Text -> Text
dropText text n =
  let len = T.length text
      check = T.take len n
  in  if check == text
      then T.drop len n
      else error $ "dropText: " ++ T.unpack n ++ "does not start with " ++ T.unpack text ++ "."

dropFuthark     = dropText "futhark_"
dropEntry       = dropText "entry_"
fromPointer     = T.dropWhile (/='*')
beforePointer   = T.takeWhile (/='*')
withoutConst    = dropWhile (=="const")
withoutUnsigned = dropWhile (=="unsigned")

mapTail g f (a:as) = g a:map f as
mapTail g f [] = []

pointerLevel :: Int -> Text -> Text
pointerLevel i a = if i > 0
                   then "Ptr " <> wrapIfNotOneWord (pointerLevel (i-1) a)
                   else a

isStruct :: [Text] -> Bool
isStruct = startsWith ["struct"]

data NameSource = Raw | Api

futharkPrimitiveToHaskellType :: Text -> Text
futharkPrimitiveToHaskellType primitive =
  case lookup primitive primTable of
    Just  s -> s
    Nothing -> error $ "type \'" ++ T.unpack primitive ++ "\' not found."

simpleTypeToHaskell :: NameSource -> Text -> Text
simpleTypeToHaskell src simple =
  let ts = withoutUnsigned $ withoutConst $ T.words $ simple
  in  if isStruct ts
      then let part = ts !! 1
               cropPart = case src of
                             Raw -> part
                             Api -> dropFuthark part
           in  cropPart
      else futharkPrimitiveToHaskellType (head ts)

foreignTypeToHaskell :: NameSource -> Type -> Text
foreignTypeToHaskell nameSource ty =
  let cType = case ty of
                TypeArray  {} -> tyArrayCType  ty
                TypeOpaque {} -> tyOpaqueCType ty
      simple = beforePointer cType
  in  simpleTypeToHaskell nameSource simple

typeToHaskell :: Manifest -> NameSource -> Int -> Int -> Text -> Text
typeToHaskell manifest nameSource primLevel arrayLevel ty =
    let eType = lookupType manifest ty
    in
    case eType of
      Right text ->
        pointerLevel primLevel $ simpleTypeToHaskell nameSource $ text
      Left ty ->
        pointerLevel arrayLevel $ foreignTypeToHaskell nameSource ty

lookupType :: Manifest -> T.Text -> Either Type T.Text
lookupType manifest text =
  case manifestTypes manifest !? text of
    Nothing -> Right text
    Just ty -> Left  ty

foreignDataDeclaration :: Type -> [Text]
foreignDataDeclaration ty =
    let name = capitalize $ foreignTypeToHaskell Raw ty
    in  ["data " <> capitalize name]

foreignImportCall :: Text -> [Text] -> Text -> [Text]
foreignImportCall futName params ret =
    let ctx       = pointerLevel 1 "Futhark_context"
    in
    [ "foreign import ccall unsafe \"" <> futName <> "\""
    , "  " <> dropFuthark futName
    ]
    ++ mapTail ("    :: " <>) ("    -> " <>)
       (ctx:params <> ["IO " <> wrapIfNotOneWord ret])

primPointer = pointerLevel 1 . futharkPrimitiveToHaskellType . tyArrayElemType

arrayNewCall :: Type -> [Text]
arrayNewCall ty =
  let futName     = arrayNew . tyArrayOps $ ty
      arrayType   = primPointer ty
      shapeParams = replicate (tyArrayRank ty) "Int64"
      returnType  = pointerLevel 1 $ foreignTypeToHaskell Raw ty
  in  foreignImportCall futName (arrayType:shapeParams) returnType

arrayFreeCall :: Type -> [Text]
arrayFreeCall ty =
  let futName     = arrayFree . tyArrayOps $ ty
      inputType   = pointerLevel 1 $ foreignTypeToHaskell Raw ty
      returnType  = "Int"
  in  foreignImportCall futName [inputType] returnType

arrayValuesCall :: Type -> [Text]
arrayValuesCall ty =
  let futName     = arrayValues . tyArrayOps $ ty
      inputType   = pointerLevel 1 $ foreignTypeToHaskell Raw ty
      outputType  = primPointer ty
      returnType  = "Int"
  in  foreignImportCall futName [inputType, outputType] returnType

arrayShapeCall :: Type -> [Text]
arrayShapeCall ty =
  let futName     = arrayShape . tyArrayOps $ ty
      inputTypes  = pointerLevel 1 $ foreignTypeToHaskell Raw ty
      returnType  = pointerLevel 1 "Int64"
  in  foreignImportCall futName [inputTypes] returnType

opaqueFreeCall    :: Type -> [Text]
opaqueFreeCall    ty =
  let futName    = opaqueFree . tyOpaqueOps $ ty
      inputTypes = [ pointerLevel 1 $ foreignTypeToHaskell Raw ty
                   ]
      returnType = "Int"
  in  foreignImportCall futName inputTypes returnType

opaqueStoreCall   :: Type -> [Text]
opaqueStoreCall   ty =
  let futName    = opaqueStore . tyOpaqueOps $ ty
      inputTypes = [ pointerLevel 1 $ foreignTypeToHaskell Raw ty
                   , pointerLevel 2 "()"
                   , pointerLevel 1 "CSize"
                   ]
      returnType = "Int"
  in  foreignImportCall futName inputTypes returnType

opaqueRestoreCall :: Type -> [Text]
opaqueRestoreCall ty =
  let futName    = opaqueRestore . tyOpaqueOps $ ty
      inputTypes = [pointerLevel 1 "()"]
      returnType = pointerLevel 1 $ foreignTypeToHaskell Raw ty
  in  foreignImportCall futName inputTypes returnType

foreignTypeDeclarations :: Text -> Type -> [Text]
foreignTypeDeclarations futTypeName ty =
    foreignDataDeclaration ty
    <>
    case ty of
      TypeArray {} ->
           arrayNewCall      ty
        <> arrayFreeCall     ty
        <> arrayValuesCall   ty
        <> arrayShapeCall    ty
      TypeOpaque {} ->
           opaqueFreeCall    ty
        <> opaqueStoreCall   ty
        <> opaqueRestoreCall ty

foreignEntryDeclaration :: Manifest -> Text -> EntryPoint -> [Text]
foreignEntryDeclaration manifest _futEntryName entry =
    let futName     = entryPointCFun entry
        outputTypes = map (capitalize . typeToHaskell manifest Raw 1 2 . outputType) $ entryPointOutputs entry
        inputTypes  = map (capitalize . typeToHaskell manifest Raw 0 1 . inputType ) $ entryPointInputs entry
        params      = outputTypes <> inputTypes
    in  foreignImportCall futName params "Int"

rawImportString :: Manifest -> [Text]
rawImportString manifest =
  concatMap (uncurry (foreignEntryDeclaration manifest)) (M.assocs $ manifestEntryPoints manifest)
  ++
  concatMap (uncurry foreignTypeDeclarations) (M.assocs $ manifestTypes manifest)

instanceDeclarations :: Type -> [Text]
instanceDeclarations ty =
  let rawName =capitalize $ tr "raw" $ foreignTypeToHaskell Raw ty
      apiName = tr "api" $ foreignTypeToHaskell Api ty
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
  [ "data " <> constructorName <> {-" c"-} " = " <> constructorName <> " (MV.MVar Int) (F.ForeignPtr Raw." <> rawName <> ")"
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
instanceDeclarationLines manifest = concatMap instanceDeclarations $ M.elems (manifestTypes manifest)

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
  then T.concat ls
  else "(" <> T.intercalate ", " ls <> ")"

arrowBefore :: Text -> Text
arrowBefore x = "  -> " <> x

entryCall :: Manifest -> EntryPoint -> [Text]
entryCall manifest entryPoint =
         typeDeclaration
      ++ input
      ++ map arrowBefore (   preCall
                          ++ call
                         )
      ++ postCall
      ++ [""]
    where
        apiName = dropFuthark (entryPointCFun entryPoint)
        entryName = dropEntry apiName
        isForeignI a = isJust $ M.lookup (inputType  a) (manifestTypes manifest)
        isForeignO a = isJust $ M.lookup (outputType a) (manifestTypes manifest)
        inArgs   = entryPointInputs  entryPoint
        outArgs  = entryPointOutputs entryPoint
        inArgTys    = map inputType  inArgs
        inArgNames  = map inputName  inArgs
        outArgTys   = map outputType outArgs
        outArgNames = map (("out"<>) . T.pack . show) [0..length outArgTys-1]
        inputHaskTypes  = map (capitalize . typeToHaskell manifest Api 0 0 . inputType) inArgs
        outputHaskTypes = map (capitalize . typeToHaskell manifest Api 0 0 . outputType) outArgs
        foreignInputs = filter isForeignI inArgs
        typeDeclaration = [entryName] ++
                          (typeWithConstraints ["Monad m"] $
                              inputHaskTypes
                              ++ ["FutT m " <> mightTuple outputHaskTypes
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
entryCallLines manifest = concat . map (entryCall manifest) $ M.elems . manifestEntryPoints $ manifest
