module ReadHeader where

import Data.Maybe
import Data.List (intercalate, partition, lookup)
import Data.Char (toUpper)
import Text.ParserCombinators.ReadP

data Arg =
  Arg
  { argType :: String
  , argName :: String
  } deriving Show

data HeaderItem
    = Preproc String
    | Comment String
    | Fun Arg [Arg]
    | Var Arg
    deriving Show

readExtern :: ReadP HeaderItem
readExtern = fmap Preproc $ (string "extern") >> manyTill get (char '\n')

readExtern2 = fmap Preproc $ (char '}') >> manyTill get (char '\n')

isWhiteSpace :: Char -> Bool
isWhiteSpace = (flip elem) " \t\n"

isNameChar :: Char -> Bool
isNameChar = not.(flip elem) " \t\n;,()"

readPreproc :: ReadP HeaderItem
readPreproc = fmap Preproc $ (char '#') >> manyTill get (char '\n')

readComment :: ReadP HeaderItem
readComment = fmap Comment $ (string "/*") >> manyTill get (string "*/")

readOnelineComment :: ReadP HeaderItem
readOnelineComment = fmap Comment $ (string "//") >> manyTill get (char '\n')

readTypeName :: ReadP Arg
readTypeName = skipSpaces
            >> sepBy (munch1 (isNameChar)) (skipMany1 $ satisfy isWhiteSpace) >>= \ws
            -> case ws of
                  []  -> return $ Arg "void" ""
                  [a] -> return $ Arg a      ""
                  a   -> return $ Arg (intercalate " " (init a) ++ takeWhile (=='*') (last a))
                                      (dropWhile (=='*') (last a) )

readVar :: ReadP HeaderItem
readVar = readTypeName >>= \tn -> skipSpaces >> char ';' >> return (Var tn)

readFun :: ReadP HeaderItem
readFun = readTypeName >>= \tn
       -> skipSpaces
       >> char '(' >> sepBy readTypeName (char ',') >>= \args -> char ')'
       >> skipSpaces >> char ';'
       >> return (Fun tn args)

readHeaderItem :: ReadP HeaderItem
readHeaderItem =   skipSpaces
               >>  readExtern
               <++ readExtern2
               <++ readPreproc
               <++ readComment
               <++ readOnelineComment
               <++ readFun
               <++ readVar

readHeader :: FilePath -> IO [HeaderItem]
readHeader fn = fmap (fst . head
            . (readP_to_S $ many readHeaderItem >>= \his
                         -> skipSpaces >> eof >> return his))
            $ readFile fn
