module Conversion where

import Data.Maybe
import Data.List (intercalate, partition, lookup)
import Data.Char (toUpper)
import Text.ParserCombinators.ReadP

data HeaderItem
    = Preproc String
    | Comment String
    | Fun (String, String) [(String, String)]
    | Var (String, String)
    deriving Show

readExtern :: ReadP HeaderItem
readExtern = fmap Preproc $ (string "extern") >> manyTill get (char '\n')

readExtern2 = fmap Preproc $ (char '}') >> manyTill get (char '\n')

isWhiteSpace :: Char -> Bool
isWhiteSpace = (flip elem) " \t\n"
isNameChar = not.(flip elem) " \t\n;,()"

readPreproc :: ReadP HeaderItem
readPreproc = fmap Preproc $ (char '#') >> manyTill get (char '\n')
readComment :: ReadP HeaderItem
readComment = fmap Comment $ (string "/*") >> manyTill get (string "*/")
readOnelineComment = fmap Comment $ (string "//") >> manyTill get (char '\n')
readTypeName = skipSpaces
            >> sepBy (munch1 (isNameChar)) (skipMany1 $ satisfy isWhiteSpace) >>= \ws
            -> case ws of
                  []  -> return ("void", "")
                  [a] -> return (a, "")
                  a   -> return ( intercalate " " (init a) ++ takeWhile (=='*') (last a)
                                , dropWhile (=='*') (last a) )

readVar :: ReadP HeaderItem
readVar = readTypeName >>= \tn -> skipSpaces >> char ';' >> return (Var tn)
readFun :: ReadP HeaderItem
readFun = readTypeName >>= \tn
       -> skipSpaces
       >> char '(' >> sepBy readTypeName (char ',') >>= \args -> char ')'
       >> skipSpaces >> char ';'
       >> return (Fun tn args)


readHeaderItem :: ReadP HeaderItem
readHeaderItem = skipSpaces >> readExtern <++ readExtern2 <++ readPreproc <++ readComment <++ readOnelineComment <++ readFun <++ readVar
readHeader fn = fmap (fst . head
            . (readP_to_S $ many readHeaderItem >>= \his
                         -> skipSpaces >> eof >> return his))
            $ readFile fn

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

haskellDeclaration :: HeaderItem -> String
haskellDeclaration item =
    case item of
        Preproc s -> ""
        Comment s -> intercalate "\n"
                     $ map (("--"++).dropWhile (==' ')) $ filter (/="") $ lines s
        Var (_, n) -> "data " ++ capitalize n
        Fun (ot, name) args -> "foreign import ccall unsafe \"" ++ name ++ "\"\n  "
                               ++ drop 8 name ++ "\n    :: "
                               ++ intercalate "\n    -> "
                                  ( (map haskellType $ filter (/="void") $ map fst args)
                                  ++ ["IO " ++ wrapIfNotOneWord (haskellType ot)] )
                               ++ "\n"

rawImportString :: [HeaderItem] -> String
rawImportString headerItems = intercalate "\n" $ map haskellDeclaration headerItems

dropString string n =
  let len = length string
      check = take len n
  in  if check == string
      then drop len n
      else error $ "dropString: " ++ n ++ "does not start with " ++ string ++ "."

dropFuthark = dropString "futhark_"
startsWith string n = take (length string) n == string

instanceDeclarations :: HeaderItem -> String
instanceDeclarations (Var (_, n)) =
  let rawName = capitalize n
      suffix = dropFuthark n
      constructorName = capitalize suffix
      notOpaque = not $ startsWith "opaque"  suffix
      isObject  = not $ startsWith "context" suffix
      isArray   = isObject && notOpaque
  in  (if isObject then objectDeclaration n else "") ++
      (if isArray  then arrayDeclaration  n else "")
instanceDeclarations _ = ""

arrayDeclaration :: String -> String
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
      arrayString = "instance FutharkArray "++ constructorName ++ " Raw."++ rawName
                 ++ " M.Ix" ++ show dim ++ " " ++ element ++ " where\n"
                 ++ "  shapeFA  = to" ++ show dim ++ "d Raw.shape_" ++ suffix ++ "\n"
                 ++ "  newFA    = from" ++ show dim ++ "d Raw.new_" ++ suffix ++ "\n"
                 ++ "  valuesFA = Raw.values_" ++ suffix ++ "\n"
  in arrayString

objectDeclaration :: String -> String
objectDeclaration n =
  let rawName = capitalize n
      suffix = dropFuthark n
      constructorName = capitalize suffix
      objectString = "\ndata " ++ constructorName ++ {-" c-} " = " ++ constructorName ++ " (MV.MVar Int) (F.ForeignPtr Raw." ++ rawName ++ ")\n"
                  ++ "instance FutharkObject " ++ constructorName ++ " Raw." ++ rawName ++ " where\n"
                  ++ "  wrapFO = " ++ constructorName ++ "\n"
                  ++ "  freeFO = Raw.free_" ++ suffix ++ "\n"
                  ++ "  fromFO (" ++ constructorName ++ " rc fp) = (rc, fp)\n"
  in  objectString

instanceDeclarationString :: [HeaderItem] -> String
instanceDeclarationString headerItems = concatMap instanceDeclarations headerItems

haskellType' :: String -> String
haskellType' s =
    let pn = length $ fromPointer s
        ts = withoutConst $ words $ beforePointer s
     in if isStruct ts
        then capitalize (dropFuthark $ ts !! 1) -- ++ " c"
        else (case lookup (head ts) varTable of
                Just s -> s;
                Nothing -> error $ "type' " ++ s ++ "not found";)

entryCall :: HeaderItem -> String
entryCall (Fun (_, n) args) =
    if isEntry
    then "\n" ++ typeDeclaration ++ input ++ preCall ++ call ++ postCall
    else ""
    where
        suffix = dropFuthark n
        isEntry = take 5 suffix == "entry"
        en = drop 6 suffix
        isFO a = case lookup (takeWhile (/='*') $ last $ words $ fst a) varTable
                    of Just _ -> False; Nothing -> True;
        (inArgs, outArgs) = partition ((=="in").take 2.snd) $ tail args
        typeDeclaration = en ++ "\n  :: Monad m \n  => "
                       ++ concatMap (\i -> haskellType' (fst i) ++ "\n  -> " ) inArgs
                       ++ "FutT m " ++ wrapIfNotOneWord (intercalate ", " $ map (\o -> haskellType' $ fst o) outArgs) ++ "\n"
        input = unwords (en : map snd inArgs) ++ "\n  =  Fut.unsafeLiftFromIO $ \\context\n  -> "
        preCall = concat
                $ map (\i -> "T.withFO " ++ snd i ++ " $ \\" ++ snd i ++ "'\n  -> ") (filter isFO inArgs)
               ++ map (\o -> "F.malloc >>= \\" ++ snd o ++ "\n  -> ") outArgs
        call = "C.inContextWithError context (\\context'\n  -> Raw." ++ suffix ++ " context' "
            ++ unwords ((map snd $ outArgs) ++ (map (\i -> if isFO i then snd i ++ "'" else snd i) inArgs)) ++ ")\n  >> "
        peek o = if isFO o then "U.peekFreeWrapIn context " else "U.peekFree "
        postCall = (if length outArgs > 1
                    then  concatMap (\o -> peek o ++ snd o ++ " >>= \\" ++ snd o ++ "'\n  -> ") outArgs
                          ++ "return " ++ wrapIfNotOneWord (intercalate ", " $ map (\o -> snd o ++ "'") outArgs)
                    else peek (head outArgs) ++ snd (head outArgs)
                   )
                ++ "\n"
entryCall _ = ""

entryCallString :: [HeaderItem] -> String
entryCallString headerItems = concatMap entryCall headerItems
