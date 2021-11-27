module Conversion where

import Data.Maybe
import Data.List (intercalate, partition, lookup)
import Data.Char (toUpper)
import Text.ParserCombinators.ReadP
import ReadHeader

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
isStruct ts     = head ts == "struct"

haskellType :: String -> String
haskellType s =
    let pn = length $ fromPointer s
        ts = withoutUnsigned $ withoutConst $ words $ beforePointer s
     in (intercalate "(" $ replicate pn "Ptr ")
     ++ (if isStruct ts
         then capitalize $ ts !! 1
         else (case lookup (head ts) varTable of
                 Just s -> s;
                 Nothing -> error $ "type \'" ++ s ++ "\' not found " ++ show ts;))
     ++ replicate (pn-1) ')'

mapTail g f (a:as) = g a:map f as
mapTail g f [] = []

haskellDeclaration :: HeaderItem -> [String]
haskellDeclaration item =
    case item of
        Preproc s -> []
        Comment s -> map (("--"++).dropWhile (==' ')) $ filter (/="") $ lines s
        Var (Arg _  n   ) -> ["data " ++ capitalize n]
        Fun (Arg ot name) args ->
            [ "foreign import ccall unsafe \"" ++ name ++ "\""
            , "  " ++ dropFuthark name
            ] ++
            mapTail ("    :: " ++) ("    -> " ++)
                ( (map haskellType $ filter (/="void") $ map argType args)
                  ++ ["IO " ++ wrapIfNotOneWord (haskellType ot)] )


rawImportString :: [HeaderItem] -> [String]
rawImportString headerItems = concatMap haskellDeclaration headerItems


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
