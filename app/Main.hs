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
    , ("half", "Half")
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
    , "import Foreign.Ptr (Ptr)" ]
    ++ map haskellDeclaration headerItems

ioWrapper (Var (_, n))
    = "newtype " ++ cn ++ " = " ++ cn ++ " (F.ForeignPtr Raw." ++ cn ++ ")\n"
    ++"instance FutharkObject " ++ cn ++ " where\n"
    ++"  freeFO = Raw.free_" ++ sn ++ "\n"
    ++"  withFO (" ++ cn ++ " fp) = F.withForeignPtr fp"
    ++ if isArray
            then "instance FutharkArray " ++ cn ++" where\n"
              ++ "  shape  = to" ++ show dim ++ "d Raw.shape_" ++ sn ++ "\n"
              ++ "  new    = from" ++ show dim ++ "d Raw.new_" ++ sn ++ "\n"
              ++ "  values = Raw.values_" ++ sn ++ "\n"
            else ""
    where cn = capitalize n
          sn = drop 8 n
          isArray = take 7 sn /= "context" && take 6 sn /= "opaque"
          dim = if isArray 
                    then read $ (:[]) $ last $ init sn
                    else 0 
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
    readHeader "rigid.h" >>= putStrLn.rawImportString


