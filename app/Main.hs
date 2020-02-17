module Main where

--import Data.List.Split (endBy, wordsBy)
import System.IO
import Data.Maybe
import Debug.Trace
import Data.List (intercalate, partition, lookup)
import Data.Char (toUpper)
import Text.ParserCombinators.ReadP

data HeaderItem 
    = Preproc String
    | Comment String
    | Fun (String, String) [(String, String)]
    | Var (String, String)
    deriving Show


isWhiteSpace = (flip elem) " \t\n"
isNameChar = not.(flip elem) " \t\n;,()"
readPreproc = fmap Preproc $ (char '#') >> manyTill get (char '\n')
readComment = fmap Comment $ (string "/*") >> manyTill get (string "*/")
readTypeName = skipSpaces
            >> sepBy (munch1 (isNameChar)) (skipMany1 $ satisfy isWhiteSpace) >>= \ws 
            -> case ws of
                  []  -> return ("void", "")
                  [a] -> return (a, "")
                  a   -> return ( intercalate " " (init a) ++ takeWhile (=='*') (last a)
                                , dropWhile (=='*') (last a) )
                    
readVar = readTypeName >>= \tn -> skipSpaces >> char ';' >> return (Var tn)
readFun = readTypeName >>= \tn 
       -> skipSpaces  
       >> char '(' >> sepBy readTypeName (char ',') >>= \args -> char ')' 
       >> skipSpaces >> char ';' 
       >> return (Fun tn args)


readHeaderItem = skipSpaces >> readPreproc <++ readComment <++ readFun <++ readVar
readHeader fn = fmap (fst . head 
            . (readP_to_S $ many readHeaderItem >>= \his 
                         -> skipSpaces >> eof >> return his)) 
            $ readFile fn

varTable = 
    [ ("int", "Int")
    , ("float", "Float")
    , ("double", "Double")
    , ("char", "Word8")
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
    , ("cl_mem", "CL_mem")
    , ("cl_command_queue", "CL_command_queue") ]


varTable2 = 
    [ ("f32", "Float")
    , ("f64", "Double")
    , ("bool", "CBool")
    , ("i8" , "Int8")
    , ("i16", "Int16")
    , ("i32", "Int32")
    , ("i64", "Int64")
    , ("u8" , "Word8")
    , ("u16", "Word16")
    , ("u32", "Word32")
    , ("u64", "Word64") ]

capitalize (c:cs) = toUpper c:cs
wrapIfNotOneWord s = if elem ' ' s then "(" ++ s ++ ")" else s
haskellType s = 
    let pn = length $ dropWhile (/='*') s
        ts = dropWhile (=="const") $ words $ takeWhile (/='*') s
     in (intercalate "(" $ replicate pn "Ptr ") 
     ++ (if head ts == "struct" 
            then capitalize $ ts !! 1
            else (case lookup (head ts) varTable of 
                    Just s -> s; 
                    Nothing -> error $ "type " ++ s ++ "not found";))
     ++ replicate (pn-1) ')'

haskellDeclaration (Preproc s) = ""
haskellDeclaration (Comment s) 
    = intercalate "\n" 
    $ map (("--"++).drop 2) $ filter (/="") $ lines s 
haskellDeclaration (Var (_, n)) = "data " ++ capitalize n
haskellDeclaration (Fun (ot, name) args) 
    =  "foreign import ccall unsafe \"" ++ name ++ "\"\n  "
    ++ drop 8 name ++ "\n    :: "
    ++ intercalate "\n    -> " 
       ( (map haskellType $ filter (/="void") $ map fst args)
       ++ ["IO " ++ wrapIfNotOneWord (haskellType ot)] )
    ++ "\n"

rawImportString headerItems = intercalate "\n" $ 
    [ "{-# LANGUAGE ForeignFunctionInterface #-}"
    , "module FutharkImports.Raw where"
    , "import Data.Int (Int8, Int16, Int32, Int64)"
    , "import Data.Word (Word8)"
    , "import Foreign.CTypes (CBool)"
    , "import Foreign.Ptr (Ptr)" ]
    ++ map haskellDeclaration headerItems

instanceDeclarations (Var (_, n))
    =  (if isObject then objectString else "") 
    ++ (if isArray  then arrayString  else "")
    where cn = capitalize sn
          sn = drop 8 n
          isObject = take 7 sn /= "context" 
          isArray = isObject && take 6 sn /= "opaque"
          dim = if isArray 
                    then read $ (:[]) $ last $ init sn
                    else 0
          element = if isArray
                        then case lookup (takeWhile (/= '_') sn) varTable2 of
                                (Just t) -> t
                                Nothing  -> error $ "ArrayType" ++ sn ++ " not found."
                        else ""
          arrayString = "instance FutharkArray (" ++ cn ++ " c) M.Sz" ++ show dim ++ " " ++ element ++ " where\n"
                     ++ "  shape  = to" ++ show dim ++ "d Raw.shape_" ++ sn ++ "\n"
                     ++ "  new    = from" ++ show dim ++ "d Raw.new_" ++ sn ++ "\n"
                     ++ "  values = Raw.values_" ++ sn ++ "\n"
          objectString = "\nnewtype " ++ cn ++ " c = " ++ cn ++ " (F.ForeignPtr Raw." ++ cn ++ ")\n"
                      ++ "instance FutharkObject (" ++ cn ++ " c) Raw." ++ cn ++ " where\n"
                      ++ "  wrapper = " ++ cn ++ "\n"
                      ++ "  freeFO = Raw.free_" ++ sn ++ "\n"
                      ++ "  withFO (" ++ cn ++ " fp) = F.withForeignPtr fp\n"

instanceDeclarations _ = ""

instanceDeclarationString headerItems = concat $
    [ "{-# LANGUAGE RankNTypes #-}"
    , "module Types where\n"
    , "import " ++ moduleName ++ ".Context\n"
    , "import " ++ moduleName ++ ".Objects\n"
    , "import qualified " ++ moduleName ++ ".Raw as Raw\n"
    , "import qualified Foreign as F\n"
    , "import qualified Data.Massiv.Array as M\n"]
    ++ map instanceDeclarations headerItems

haskellType' s = 
    let pn = length $ dropWhile (/='*') s
        ts = dropWhile (=="const") $ words $ takeWhile (/='*') s
     in if head ts == "struct" 
            then capitalize (drop 8 $ ts !! 1) ++ " c"
            else (case lookup (head ts) varTable of 
                    Just s -> s; 
                    Nothing -> error $ "type " ++ s ++ "not found";)

entryCall (Fun (_, n) args) 
    = if isEntry 
        then "\n" ++ typeDeclaration ++ input ++ preCall ++ call ++ postCall
        else ""
    where
        sn = drop 8 n
        isEntry = take 5 sn == "entry"
        en = drop 6 sn
        isFO a = case lookup (takeWhile (/='*') $ last $ words $ fst a) varTable 
                    of Just _ -> False; Nothing -> True; 
        (inArgs, outArgs) = partition ((=="in").take 2.snd) $ tail args
         typeDeclaration = en ++ "\n  :: " 
                       ++ concatMap (\i -> haskellType' (fst i) ++ "\n  -> " ) inArgs
                       ++ "FT c " ++ wrapIfNotOneWord (intercalate ", " $ map (\o -> haskellType' $ fst o) outArgs) ++ "\n"
        input = unwords (en : map snd inArgs) ++ "\n  =  FT.unsafeLiftFromIO $ \\context\n  -> "
        preCall = concat 
                $ map (\i -> "C.withFO " ++ snd i ++ " $ \\" ++ snd i ++ "'\n  -> ") (filter isFO inArgs)
               ++ map (\o -> "F.malloc >>= $ \\" ++ snd o ++ "\n  -> ") outArgs 
        call = "C.inContextWithError context (\\context'\n  -> Raw." ++ sn ++ " context' " 
            ++ unwords ((map snd $ outArgs) ++ (map (\i -> if isFO i then snd i ++ "'" else snd i) inArgs)) ++ ")\n  >> "
        peek o = if isFO o then "C.peekFreeWrap " else "C.peekFree "
        postCall = (if length outArgs > 1
                        then  concatMap (\o -> peek o ++ snd o ++ " >>= \\" ++ snd o ++ "'\n  -> ") outArgs
                          ++ "return " ++ wrapIfNotOneWord (intercalate ", " $ map (\o -> snd o ++ "'") outArgs)
                        else peek (head outArgs) ++ snd (head outArgs))
                ++ "\n"

entryCall _ = ""
        
entryCallString moduleName headerItems = concat $
    --[ "{-# LANGUAGE #-}\n"
    [ "module Entries where\n"
    , "import qualified " ++ moduleName ++ ".Context as C\n"
    , "import qualified " ++ moduleName ++ ".FT as FT\n"
    , "import qualified " ++ moduleName ++ ".Raw as Raw\n"
    , "import " ++ moduleName ++ ".Types\n"
    , "import qualified Foreign as F\n" ]
    ++ map entryCall headerItems

data Import = N String | Q String String

globalImport (N m) = "import " ++ m ++ "\n"
globalImport (Q m a) = "import qualified " ++ m ++ " as " ++ a ++ "\n"
localImport moduleName (N sub) = globalImport $ N (moduleName ++ "." ++ sub)
localImport moduleName (Q sub a) = globalImport $ Q (moduleName ++ "." ++ sub) a

haskellHeader extensions localImports globalImports subModuleName moduleName
    =  (if length extensions > 1 
        then "{-# LANGUAGE " ++ intercalate ", " extensions ++ " #-}" 
        else "")
    ++ "\nmodule " ++ moduleName ++ (case subModuleName of Nothing -> ""; Just n -> '.':n) ++ " where\n"
    ++ concatMap (localImport moduleName) localImports
    ++ concatMap globalImport globalImports

rawHeader     = haskellHeader ["ForeignFunctionInterface"] [] 
    [ N "Data.Int (Int8, Int16, Int32, Int64)"
    , N "Data.Word (Word8, Word16, Word32 Word64)"
    , N "Foreign.CTypes (CBool)"
    , N "Foreign.Ptr (Ptr)" ] (Just "Raw")
entryHeader   = haskellHeader [] [Q "Raw" "Raw", Q "Context" "C", Q "FT" "FT"] [Q "Foreign" "F"] (Just "Entries")
configHeader  = haskellHeader [] [Q "Raw" "Raw"] [] (Just"Config")
contextHeader = haskellHeader ["RankNTypes"] [] [] (Just "Context")
objectHeader  = haskellHeader ["RankNTypes"] [] [] (Just "Object")
typesHeader   = haskellHeader ["RankNTypes"] [] [] (Just "Object")
fTHeader      = haskellHeader [] [N "Context"] [] (Just "FT (runFTIn, runFTWith, runFT, unsafeLiftFromIO)")
exportHeader  = haskellHeader [] [] [] Nothing


main :: IO ()
main = do
    putStrLn "test"
    --text <- readFile "rigid.h"
    --mapM_ (putStrLn.declaration.parseLine) $ cleanLines $ lines text
    header <- readHeader "rigid.h"
    putStrLn $ rawImportString header
    putStrLn $ instanceDeclarationString header
    putStrLn $ entryCallString header

