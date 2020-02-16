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
    , "module Instances\n"
    , "import Context\n"
    , "import Objects\n"
    , "import qualified Raw\n"
    , "import qualified Data.Massiv.Array as M\n"]
    ++ map instanceDeclarations headerItems

entryCall (Fun (_, n) args) 
    = if isEntry 
        then "\n" ++ input ++ preCall ++ call ++ postCall
        else ""
    where
        sn = drop 8 n
        isEntry = take 5 sn == "entry"
        isFO a = case lookup (takeWhile (/='*') $ last $ words $ fst a) varTable 
                    of Just _ -> False; Nothing -> True; 
        (inArgs, outArgs) = partition ((=="in").take 2.snd) $ tail args
        input = unwords (sn : map snd inArgs) ++ " = unsafeLiftFromIO $ \\context\n  -> "
        preCall = concat 
                $ map (\i -> "withFO " ++ snd i ++ " $ \\" ++ snd i ++ "'\n  -> ") (filter isFO inArgs)
               ++ map (\o -> "malloc >>= $ \\" ++ snd o ++ "\n  -> ") outArgs 
        call = "inContextWithError context (\\c -> Raw." ++ sn ++ " c " 
            ++ unwords ((map snd $ outArgs) ++ (map (\i -> if isFO i then snd i ++ "'" else snd i) inArgs)) ++ ")\n  >> "
        postCall = concatMap (\o -> (if isFO o then "peekFreeAndWrap " else "peekAndFree ") 
                ++ snd o ++ " >>= \\" ++ snd o ++ "'\n  -> ") outArgs
                ++ "return " ++ wrapIfNotOneWord (intercalate ", " $ map (\o -> snd o ++ "'") outArgs) ++ "\n"

entryCall _ = ""
        
entryCallString headerItems = concat $
    [ "{-# LANGUAGE #-}\n"
    , "import Context\n"
    , "import FT\n" ]
    ++ map entryCall headerItems
{--
wrapper (Type s) = "type " ++ capitalize (drop 8 s) ++ "= ForeignPtr " ++ capitalize s
wrapper (Function (name, ot) args) = 
    let preCall = catMaybes
                $ map (\arg -> case arg of
                      ()) args
        baseCall = 
        call = case ot of
            "()" -> baseCall
            "()"
        postCall = 
--}
main :: IO ()
main = do
    putStrLn "test"
    --text <- readFile "rigid.h"
    --mapM_ (putStrLn.declaration.parseLine) $ cleanLines $ lines text
    header <- readHeader "rigid.h"
    putStrLn $ rawImportString header
    putStrLn $ instanceDeclarationString header
    putStrLn $ entryCallString header

