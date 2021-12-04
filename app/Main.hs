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
    putStrLn "futhask version 0.2.2"
    args <- getArgs
    [ backendS, jsonName, srcDir, moduleName ] <- getArgs >>= \args -> case args of
         [a, b, c, d] -> return args
         _            -> error "futhask takes four arguments:\n - backend (c, opencl, cuda)\n - Futhark JSON file\n - Haskell source directory\n - module name"
    createDirectoryIfMissing False (srcDir ++ "/" ++ moduleName)
    manifest <- readManifest jsonName
    -- backend <- case backendS of
    --     "c"      -> return C
    --     "opencl" -> return OpenCL
    --     "cuda"   -> return Cuda
    --     _        -> error $ "unknown backend: " ++ backendS ++ "\n  available backends: c, opencl, cuda"
    let backend   = manifestBackend manifest
    let typeMap   = buildFutharkTypes manifest
    let types     = M.elems typeMap
    let entries   = M.elems $ buildEntries typeMap manifest
    dataWrappers   <- foreignDataWrappers         types
    typeOps        <- foreignTypeOpDeclarations   types
    instances      <- haskellInstanceDeclarations types
    foreignEntries <- foreignEntryDeclarations    entries
    haskellTypes   <- haskellDataWrappers         types
    haskellEntries <- haskellEntryDeclarations    entries
    mapM_ (writeModule backend srcDir moduleName)
        [ (Just "Raw", rawHeader, commonRawBody ++ dataWrappers ++ foreignEntries ++ typeOps)
        , (Just "Entries", entriesHeader, haskellEntries)
        , (Just "Types", typesHeader, haskellTypes ++ instances)
        , (Just "TypeClasses", typeClassesHeader, typeClassesBody)
        , (Just "Context", contextHeader, contextBody)
        , (Just "Config", configHeader, configBody backend)
        , (Just "Fut", futHeader, futBody)
        , (Just "Wrap", wrapHeader, wrapBody)
        , (Just "Utils", utilsHeader, utilsBody)
        , (Nothing, exportsHeader, "") ]
