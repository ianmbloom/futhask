{-# LANGUAGE OverloadedStrings #-}

module Generate

where

import Data.Maybe (fromJust)
import Data.Char (toUpper)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.String (IsString(..))

import GHC.SourceGen
import GHC.Paths (libdir)
import GHC (runGhc, getSessionDynFlags)

import qualified Data.Map as M
import Manifest

import Name

encodeGHC :: [HsDecl'] -> IO String
encodeGHC x = runGhc (Just libdir) $ do
   dynFlags <- getSessionDynFlags
   return $ showPpr dynFlags x

wrapIfNotOneWord :: Text -> Text
wrapIfNotOneWord s = if length (T.words s) > 1 then "(" <> s <> ")" else s

mapTail :: (a->a) -> (a->a) -> [a] -> [a]
mapTail g f (a:as) = g a:map f as
mapTail g f []     = []

indentTail :: Text -> Text -> [Text] -> [Text]
indentTail a b = mapTail (a<>) (b<>)

pointerLevel :: Int -> Text -> Text
pointerLevel i a =
  if i > 0
  then "Ptr " <> wrapIfNotOneWord (pointerLevel (i-1) a)
  else a

inTupleMore :: [Text] -> [Text]
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

withSkolem :: (Text -> Text) -> Text -> Text
withSkolem f key =
  let suffix =
        case maybePrim key of
          Just prim -> ""
          Nothing   -> " c"
  in  f key <> suffix

foreignDataDeclaration :: Text -> [Text]
foreignDataDeclaration haskellTy =
    ["data " <> capitalize haskellTy ]

foreignImportCall :: Text -> Text ->[Text] -> Text -> [Text]
foreignImportCall arrName callName params ret =
    let ctx       = pointerLevel 1 "Futhark_context"
    in
    [ "foreign import ccall unsafe \"futhark_" <> callName <> "\""
    , "  " <> arrName
    ]
    ++ indentTail "    :: " "    -> "
       (ctx:params <> ["IO " <> wrapIfNotOneWord ret])

-- declareObject :: Text -> Text -> Text -> [Text]
-- declareObject apiName constructorName rawName =
--   [ "data " <> constructorName <> " c = " <> constructorName <> " (MV.MVar Int) (F.ForeignPtr Raw." <> rawName <> ")"
--   , "instance FutharkObject " <> constructorName <> " Raw." <> rawName <> " where"
--   , "  wrapFO = " <> constructorName
--   , "  freeFO = Raw.free_" <> apiName
--   , "  fromFO (" <> constructorName <> " rc fp) = (rc, fp)"
--   ]

declareObject :: Text -> Text -> Text -> [Text]
declareObject apiName constructorName rawName =
  let qualifiedRaw     = "Raw." <> rawName
      qualifiedRawFree = "Raw.free_" <> apiName
  in
    instance' (var "FutharkObject" @@ var constructorName @@ var qualifiedRaw)
      [ funBind "wrapFO" $ match [] $ conP constructorName  []
      , funBind "freeFO" $ match [] $ conP qualifiedRawFree []
      , funBind "fromFO" $ match [conP constructorName [var "rc", var "fp"]] $ tuple [var "rc", var "fp"]
      ]

-- declareArray :: Text -> Text -> Text -> Text -> Text -> [Text]
-- declareArray apiName constructorName rawName dimStr element =
--   [ "instance FutharkArray " <> constructorName <> " Raw." <> rawName <> " M.Ix" <> dimStr <> " " <> element <> " where"
--   , "  shapeFA  = to" <> dimStr <> "d Raw.shape_" <> apiName
--   , "  newFA    = from" <> dimStr <> "d Raw.new_" <> apiName
--   , "  valuesFA = Raw.values_" <> apiName
--   ]

declareArray :: Text -> Text -> Text -> Text -> Text -> [Text]
declareArray apiName constructorName rawName dimStr element =
  let qualifiedRaw       = "Raw."       <> rawName
      qualifiedRawShape  = "Raw.shape_" <> apiName
      qualifiedRawNew    = "Raw.new_"   <> apiName
      qualifiedRawValues = "Raw.new_"   <> apiName
      dim     = "M.Ix" <> dimStr
      toFun   = "to"   <> dimStr <> "d"
      fromFun = "from" <> dimStr <> "d"
  in  instance' (var "FutharkArray" @@ var constructorName @@ var qualifiedRaw @@ var dim @@ var element)
        [ funBind "shapeFA"  (match []) $ toFun   @@ qualifiedRawShape
        , funBind "newFA"    (match []) $ fromFun @@ qualifiedRawNew
        , funBind "valuesFA" (match []) $ qualifiedRawValues
        ]

entryCall :: Manifest -> Text -> [Text]
entryCall manifest entryName =
         [ typeDeclaration
         , preCall (call postCall)
         ]
    where
        entryPoint = lookupEntry manifest entryName
        apiName = "entry_" <> entryName
        isForeignI a = isJust $ M.lookup (inputType  a) (manifestTypes manifest)
        isForeignO a = isJust $ M.lookup (outputType a) (manifestTypes manifest)
        inArgs   = entryPointInputs  entryPoint
        outArgs  = entryPointOutputs entryPoint
        inArgTys    = map inputType  inArgs
        inArgNames  = map inputName  inArgs
        outArgTys   = map outputType outArgs
        outArgNames = map (("out"<>) . T.pack . show) [0..length outArgTys-1]
        inputHaskTypes  = map (withSkolem (capitalize . typeToHaskell manifest) . inputType ) inArgs
        outputHaskTypes = map (withSkolem (capitalize . typeToHaskell manifest) . outputType) outArgs
        foreignInputs = map inputName $ filter isForeignI inArgs

--
--         typeDeclaration = [entryName] ++
--                           (typeWithConstraints ["Monad m"] $
--                               inputHaskTypes
--                               ++ ["FutT c m " <> mightTuple outputHaskTypes
--                                  ]
--                           )
--         input = [ T.unwords (entryName : inArgNames)
--                 , "  =  Fut.unsafeLiftFromIO $ \\context"
--                 ]
--         preCall =  (map (\i -> "T.withFO " <> i <> " $ \\" <> i <> "'") foreignInputs)
--                 ++ (map (\o -> "F.malloc >>= \\" <> o) outArgNames)
--         call = [ "C.inContextWithError context (\\context'"
--                , "Raw." <> apiName <> " context' " <> T.unwords (outArgNames <> (map (\i -> inputName i <> if isForeignI i then "'" else "") inArgs)) <> ")"
--                ]
--         peek o = if isForeignO o then "U.peekFreeWrapIn context " else "U.peekFree "
--         postCall = indentTail "  >> " "  -> " $
--                    if length outArgs > 1
--                    then  zipWith (\o name -> peek o <> name <> " >>= \\" <> name <> "'") outArgs outArgNames
--                          ++ ["return " <> mightTuple (map (\o -> o <> "'") outArgNames)]
--                    else [peek (head outArgs) <> head outArgNames]
--

        prime i = i <> "'"
        typeDeclaration = typeSig entryName
                        $ var "Monad" @@ var "m"
                        ==> foldl1 (--->) (map var inputHaskTypes)
                        --> var "FutT" @@ var c @@ var m @@ tuple (map var outputHaskTypes)
        params = map var inArgNames
        mkPrecallIn  i body = lambda (var i) $ var "T.withFO" @@ var i @@ (lambda (var $ prime i) body)
        mkPrecallOut o body = lambda (var o) $ op (var "F.malloc") ">>=" (lambda (var o) body)
        preCall body = foldl mkPreCallIn (foldl mkPreCallOut body foreignInputs) outArgNames
        call = undefined
        postCall = undefined
