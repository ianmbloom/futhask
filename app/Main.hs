module Main where

import System.IO
import Data.Maybe
import Data.List (intercalate)
import qualified Data.Map as M
import Debug.Trace
import System.Directory
import System.Environment
import qualified Data.Text as T
import CodeBodies
import Declarations
import Convert
import Headers
import Manifest
import Type
import Encode

writeModule :: p
            -> String
            -> String
            -> ( Maybe String
               , p -> String -> Maybe String -> String
               , String
               )
            -> IO ()
writeModule backend directory moduleName (subModuleName, headerF, body)
    = writeFile fn string
    where fn = directory ++ "/" ++ moduleName
            ++ (case subModuleName of Just n -> "/" ++ n; Nothing -> "") ++ ".hs"
          string = headerF backend moduleName subModuleName ++ body

main :: IO ()
main = do
    putStrLn "Starting futhask version 0.2.2:"
    args <- getArgs
    [jsonName, srcDir, moduleName ] <- getArgs >>= \args -> case args of
         [a, b, c] -> return args
         _         -> error "futhask takes three arguments:\n - Futhark JSON file\n - Haskell source directory\n - module name"
    createDirectoryIfMissing False (srcDir ++ "/" ++ moduleName)
    manifest <- readManifest jsonName
    let backend = manifestBackend manifest
        typeMap = buildFutharkTypes manifest
        types   = M.elems typeMap
        entries = M.elems $ buildEntries typeMap manifest
    dataWrappers   <- foreignDataWrappers         types
    typeOps        <- foreignTypeOpDeclarations   types
    instances      <- haskellInstanceDeclarations types
    foreignEntries <- foreignEntryDeclarations    entries
    haskellTypes   <- haskellDataWrappers         types
    haskellEntries <- haskellEntryDeclarations    entries
    mapM_ (writeModule backend srcDir moduleName)
        [ (Just "Raw"        , rawHeader        ,    commonRawBody
                                                  ++ dataWrappers 
                                                  ++ foreignEntries
                                                  ++ typeOps               )
        , (Just "Entries"    , entriesHeader    , haskellEntries           )
        , (Just "Types"      , typesHeader      , haskellTypes ++ instances)
        , (Just "TypeClasses", typeClassesHeader, typeClassesBody          )
        , (Just "Context"    , contextHeader    , contextBody              )
        , (Just "Config"     , configHeader     , configBody backend       )
        , (Just "Fut"        , futHeader        , futBody                  )
        , (Just "Wrap"       , wrapHeader       , wrapBody                 )
        , (Just "Utils"      , utilsHeader      , utilsBody                )
        , (Nothing, exportsHeader, "") ]
    putStrLn "Futhask wrapper generation complete."
