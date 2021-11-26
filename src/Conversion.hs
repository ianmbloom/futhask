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

haskellType :: String -> String
haskellType s =
    let pn = length $ dropWhile (/='*') s
        ts = dropWhile (=="unsigned") $ dropWhile (=="const") $ words $ takeWhile (/='*') s
     in (intercalate "(" $ replicate pn "Ptr ")
     ++ (if head ts == "struct"
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

instanceDeclarations :: HeaderItem -> String
instanceDeclarations (Var (_, n)) =
  let rn = capitalize n
      sn = drop 8 n
      cn = capitalize sn
      notOpaque = take 6 sn /= "opaque"
      isObject  = take 7 sn /= "context"
      isArray   = isObject && notOpaque
  in  (if isObject then objectDeclaration n else "") ++
      (if isArray  then arrayDeclaration  n else "")
instanceDeclarations _ = ""

arrayDeclaration :: String -> String
arrayDeclaration n =
  let rn = capitalize n
      sn = drop 8 n
      cn = capitalize sn
      notOpaque = take 6 sn /= "opaque"
      isObject  = take 7 sn /= "context"
      isArray   = isObject && notOpaque
      dim = if isArray
            then read $ (:[]) $ last $ init sn
            else 0
      element = if isArray
                then case lookup (takeWhile (/= '_') sn) varTable2 of
                        (Just t) -> t
                        Nothing  -> error $ "ArrayType" ++ sn ++ " not found."
                else ""
      arrayString = "instance FutharkArray "++ cn ++ " Raw."++ rn
                 ++ " M.Ix" ++ show dim ++ " " ++ element ++ " where\n"
                 ++ "  shapeFA  = to" ++ show dim ++ "d Raw.shape_" ++ sn ++ "\n"
                 ++ "  newFA    = from" ++ show dim ++ "d Raw.new_" ++ sn ++ "\n"
                 ++ "  valuesFA = Raw.values_" ++ sn ++ "\n"
  in arrayString

objectDeclaration :: String -> String
objectDeclaration n =
  let rn = capitalize n
      sn = drop 8 n
      cn = capitalize sn
      objectString = "\ndata " ++ cn ++ {-" c-} " = " ++ cn ++ " (MV.MVar Int) (F.ForeignPtr Raw." ++ rn ++ ")\n"
                  ++ "instance FutharkObject " ++ cn ++ " Raw." ++ rn ++ " where\n"
                  ++ "  wrapFO = " ++ cn ++ "\n"
                  ++ "  freeFO = Raw.free_" ++ sn ++ "\n"
                  ++ "  fromFO (" ++ cn ++ " rc fp) = (rc, fp)\n"
  in  objectString

instanceDeclarationString :: [HeaderItem] -> String
instanceDeclarationString headerItems = concatMap instanceDeclarations headerItems

haskellType' :: String -> String
haskellType' s =
    let pn = length $ dropWhile (/='*') s
        ts = dropWhile (=="const") $ words $ takeWhile (/='*') s
     in if head ts == "struct"
        then capitalize (drop 8 $ ts !! 1) -- ++ " c"
        else (case lookup (head ts) varTable of
                Just s -> s;
                Nothing -> error $ "type' " ++ s ++ "not found";)

entryCall :: HeaderItem -> String
entryCall (Fun (_, n) args) =
    if isEntry
    then "\n" ++ typeDeclaration ++ input ++ preCall ++ call ++ postCall
    else ""
    where
        sn = drop 8 n
        isEntry = take 5 sn == "entry"
        en = drop 6 sn
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
        call = "C.inContextWithError context (\\context'\n  -> Raw." ++ sn ++ " context' "
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
