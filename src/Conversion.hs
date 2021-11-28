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
import Debug

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

varTable2 =
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

capitalize (c:cs) = toUpper c:cs
wrapIfNotOneWord s = if elem ' ' s then "(" ++ s ++ ")" else s

startsWith string n = take (length string) n == string
dropString string n =
  let len = length string
      check = take len n
  in  if check == string
      then drop len n
      else error $ "dropString: " ++ n ++ "does not start with " ++ string ++ "."

dropFuthark     = dropString "futhark_"
dropEntry       = dropString "entry_"
fromPointer     = dropWhile (/='*')
beforePointer   = takeWhile (/='*')
withoutConst    = dropWhile (=="const")
withoutUnsigned = dropWhile (=="unsigned")

mapTail g f (a:as) = g a:map f as
mapTail g f [] = []

pointerLevel i a = if i > 0
                   then "Ptr " ++ wrapIfNotOneWord (pointerLevel (i-1) a)
                   else a

isStruct = startsWith ["struct"]

futharkPrimitiveToHaskellType :: String -> String
futharkPrimitiveToHaskellType primitive =
  case lookup primitive varTable2 of
    Just s -> s
    Nothing -> error $ "type \'" ++ primitive ++ "\' not found."

simpleTypeToHaskell :: String -> String
simpleTypeToHaskell simple =
  let ts = withoutUnsigned $ withoutConst $ words $ simple
  in  if isStruct ts
      then capitalize $ ts !! 1
      else futharkPrimitiveToHaskellType (head ts)

arrayTypeToHaskell :: Type -> String
arrayTypeToHaskell ty =
  let cType = case ty of
                TypeArray  {} -> tyArrayCType  ty
                TypeOpaque {} -> tyOpaqueCType ty
      simple = beforePointer $ T.unpack cType
  in  simpleTypeToHaskell simple

typeToHaskell :: Int -> Int -> Either Type T.Text -> String
typeToHaskell primLevel arrayLevel eType =
    case eType of
      Right text ->
        pointerLevel primLevel $ simpleTypeToHaskell $ T.unpack text
      Left ty ->
        pointerLevel arrayLevel $ arrayTypeToHaskell ty

haskellDeclaration :: Manifest -> HeaderItem -> [String]
haskellDeclaration manifest item =
    case item of
        Preproc s -> []
        Comment s -> map (("--"++).dropWhile (==' ')) $ filter (/="") $ lines s
        Var (Arg _  name) -> ["data " ++ capitalize name]
        Fun (Arg ot name) args -> []

lookupType :: Manifest -> T.Text -> Either Type T.Text
lookupType manifest text =
  case manifestTypes manifest !? text of
    Nothing -> Right text
    Just ty -> Left  ty

foreignDataDeclaration :: Type -> [String]
foreignDataDeclaration ty =
    let name = arrayTypeToHaskell ty
    in  ["data " ++ capitalize name]

foreignImportCall :: T.Text -> [String] -> String -> [String]
foreignImportCall futName params ret =
    let futString = T.unpack futName
        ctx       = pointerLevel 1 "Futhark_context"
    in
    [ "foreign import ccall unsafe \"" ++ futString ++ "\""
    , "  " ++ dropFuthark futString
    ]
    ++ mapTail ("    :: " ++) ("    -> " ++)
       (ctx:params ++ ["IO " ++ wrapIfNotOneWord ret])

primPointer = pointerLevel 1 . futharkPrimitiveToHaskellType . T.unpack . tyArrayElemType

arrayNewCall :: Type -> [String]
arrayNewCall ty =
  let futName     = arrayNew . tyArrayOps $ ty
      arrayType   = primPointer ty
      shapeParams = replicate (tyArrayRank ty) "Int64"
      returnType  = pointerLevel 1 $ arrayTypeToHaskell ty
  in  foreignImportCall futName (arrayType:shapeParams) returnType

arrayFreeCall :: Type -> [String]
arrayFreeCall ty =
  let futName     = arrayFree . tyArrayOps $ ty
      inputType   = pointerLevel 1 $ arrayTypeToHaskell ty
      returnType  = "Int"
  in  foreignImportCall futName [inputType] returnType

arrayValuesCall :: Type -> [String]
arrayValuesCall ty =
  let futName     = arrayValues . tyArrayOps $ ty
      inputType   = pointerLevel 1 $ arrayTypeToHaskell ty
      outputType  = primPointer ty
      returnType  = "Int"
  in  foreignImportCall futName [inputType, outputType] returnType

arrayShapeCall :: Type -> [String]
arrayShapeCall ty =
  let futName     = arrayShape . tyArrayOps $ ty
      inputTypes  = pointerLevel 1 $ arrayTypeToHaskell ty
      returnType  = pointerLevel 1 "Int64"
  in  foreignImportCall futName [inputTypes] returnType

opaqueFreeCall    :: Type -> [String]
opaqueFreeCall    ty =
  let futName    = opaqueFree . tyOpaqueOps $ ty
      inputTypes = [ pointerLevel 1 $ arrayTypeToHaskell ty
                   ]
      returnType = "Int"
  in  foreignImportCall futName inputTypes returnType

opaqueStoreCall   :: Type -> [String]
opaqueStoreCall   ty =
  let futName    = opaqueStore . tyOpaqueOps $ ty
      inputTypes = [ pointerLevel 1 $ arrayTypeToHaskell ty
                   , pointerLevel 2 "()"
                   , pointerLevel 1 "CSize"
                   ]
      returnType = "Int"
  in  foreignImportCall futName inputTypes returnType

opaqueRestoreCall :: Type -> [String]
opaqueRestoreCall ty =
  let futName    = opaqueRestore . tyOpaqueOps $ ty
      inputTypes = [pointerLevel 1 "()"]
      returnType = pointerLevel 1 $ arrayTypeToHaskell ty
  in  foreignImportCall futName inputTypes returnType

foreignTypeDeclarations :: T.Text -> Type -> [String]
foreignTypeDeclarations futTypeName ty =
    foreignDataDeclaration ty
    ++
    case ty of
      TypeArray {} ->
           arrayNewCall      ty
        ++ arrayFreeCall     ty
        ++ arrayValuesCall   ty
        ++ arrayShapeCall    ty
      TypeOpaque {} ->
           opaqueFreeCall    ty
        ++ opaqueStoreCall   ty
        ++ opaqueRestoreCall ty

foreignEntryDeclaration :: Manifest -> T.Text -> EntryPoint -> [String]
foreignEntryDeclaration manifest _futEntryName entry =
    let futName     = entryPointCFun entry
        outputTypes = map (typeToHaskell 1 2 . lookupType manifest . outputType) $ entryPointOutputs entry
        inputTypes  = map (typeToHaskell 0 1 . lookupType manifest . inputType ) $ entryPointInputs entry
        params      = outputTypes ++ inputTypes
    in  foreignImportCall futName params "Int"

rawImportString :: Manifest -> [String]
rawImportString manifest =
  -- concatMap (haskellDeclaration manifest) headerItems
  -- ++
  concatMap (uncurry (foreignEntryDeclaration manifest)) (M.assocs $ manifestEntryPoints manifest)
  ++
  concatMap (uncurry foreignTypeDeclarations) (M.assocs $ manifestTypes manifest)

instanceDeclarations :: HeaderItem -> [String]
instanceDeclarations (Var (Arg _ n)) =
  let rawName = capitalize n
      suffix = dropFuthark n
      constructorName = capitalize suffix
      notOpaque = not $ startsWith "opaque"  suffix
      isObject  = not $ startsWith "context" suffix
      isArray   = isObject && notOpaque
  in  (if isObject then objectDeclaration n else []) ++
      (if isArray  then arrayDeclaration  n else [])
instanceDeclarations _ = []

arrayDeclaration :: String -> [String]
arrayDeclaration n =
  let rawName = capitalize n
      suffix = dropFuthark n
      constructorName = capitalize suffix
      notOpaque = not $ startsWith "opaque"  suffix
      isObject  = not $ startsWith "context" suffix
      isArray   = isObject && notOpaque
      dim = if isArray
            then read $ (:[]) $ last $ init suffix
            else 0
      element = if isArray
                then case lookup (takeWhile (/= '_') suffix) varTable2 of
                        (Just t) -> t
                        Nothing  -> error $ "ArrayType" ++ suffix ++ " not found."
                else ""
      dimStr = show dim
      arrayString =
        [ "instance FutharkArray "++ constructorName ++ " Raw." ++ rawName ++ " M.Ix" ++ dimStr ++ " " ++ element ++ " where"
        , "  shapeFA  = to" ++ dimStr ++ "d Raw.shape_" ++ suffix
        , "  newFA    = from" ++ dimStr ++ "d Raw.new_" ++ suffix
        , "  valuesFA = Raw.values_" ++ suffix
        ]
  in arrayString

objectDeclaration :: String -> [String]
objectDeclaration n =
  let rawName = capitalize n
      suffix = dropFuthark n
      constructorName = capitalize suffix
      objectString =
        [ ""
        , "data " ++ constructorName ++ {-" c-} " = " ++ constructorName ++ " (MV.MVar Int) (F.ForeignPtr Raw." ++ rawName ++ ")"
        , "instance FutharkObject " ++ constructorName ++ " Raw." ++ rawName ++ " where"
        , "  wrapFO = " ++ constructorName
        , "  freeFO = Raw.free_" ++ suffix
        , "  fromFO (" ++ constructorName ++ " rc fp) = (rc, fp)"
        ]
  in  objectString

instanceDeclarationLines :: [HeaderItem] -> [String]
instanceDeclarationLines headerItems = concatMap instanceDeclarations headerItems

haskellType' :: String -> String
haskellType' s =
    let pn = length $ fromPointer s
        ts = withoutConst $ words $ beforePointer s
     in if isStruct ts
        then capitalize (dropFuthark $ ts !! 1) -- ++ " c"
        else (case lookup (head ts) varTable of
                Just s -> s;
                Nothing -> error $ "type' " ++ s ++ "not found";)

indentTail a b = mapTail (a++) (b++)

inTupleMore ls =
  if length ls > 1
  then indentTail "( " ", " ls ++ [")"]
  else ls

makeConstraints :: [String] -> [String]
makeConstraints = inTupleMore

typeWithConstraints :: [String] -> [String] -> [String]
typeWithConstraints constraintLines typeLines =
  if length constraintLines > 0
  then    indentTail " :: " "    " constraintLines
       ++ indentTail " => " " -> " typeLines
  else    indentTail " :: " " -> " typeLines

mightTuple ls =
  if length ls <= 1
  then concat ls
  else "(" ++ intercalate ", " ls ++ ")"

arrowBefore x = "  -> " ++ x



entryCall :: HeaderItem -> [String]
entryCall (Fun (Arg _ n) args) =
    if isEntry
    then     typeDeclaration
          ++ input
          ++ map arrowBefore (   preCall
                              ++ call
                             )
          ++ postCall
          ++ [""]
    else []
    where
        suffix = dropFuthark n
        isEntry = startsWith "entry" suffix
        entryName = dropEntry suffix
        isFO a = not . isJust $ lookup (beforePointer $ last $ words $ argType a) varTable
        (inArgs, outArgs) = partition (startsWith "in" . argName ) $ tail args
        inArgTys    = map argType inArgs
        inArgNames  = map argName inArgs
        outArgTys   = map argType outArgs
        outArgNames = map argName outArgs
        typeDeclaration = [entryName] ++
                          (typeWithConstraints ["Monad m"] $
                              map (\i -> haskellType' (argType i)) inArgs
                              ++ ["FutT m " ++ mightTuple (map (\o -> haskellType' $ argType o) outArgs)
                                 ]
                          )
        input = [ unwords (entryName : map argName inArgs)
                , "  =  Fut.unsafeLiftFromIO $ \\context"
                ]
        preCall =  (map (\i -> "T.withFO " ++ i ++ " $ \\" ++ i ++ "'") (map argName $ filter isFO inArgs))
                ++ (map (\o -> "F.malloc >>= \\" ++ o) (map argName outArgs))
        call = [ "C.inContextWithError context (\\context'"
               , "Raw." ++ suffix ++ " context' " ++ unwords (outArgNames ++ (map (\i -> if isFO i then argName i ++ "'" else argName i) inArgs)) ++ ")"
               ]
        peek o = if isFO o then "U.peekFreeWrapIn context " else "U.peekFree "
        postCall = indentTail "  >> " "  -> " $
                   if length outArgs > 1
                   then  map (\o -> peek o ++ argName o ++ " >>= \\" ++ argName o ++ "'") outArgs
                         ++ ["return " ++ mightTuple (map (\o -> o ++ "'") outArgNames)]
                   else [peek (head outArgs) ++ head outArgNames]

entryCall _ = []

entryCallLines :: [HeaderItem] -> [String]
entryCallLines headerItems = concatMap entryCall headerItems
