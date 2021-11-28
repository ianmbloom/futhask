module Main where

import System.IO
import Data.Maybe
import Data.List (intercalate)
import Debug.Trace
import System.Directory
import System.Environment
import qualified Data.Text as T
import CodeBodies
import Conversion
import Headers
import Manifest
import Type

writeModule backend directory moduleName (subModuleName, headerF, body)
    = writeFile fn string
    where fn = directory ++ "/" ++ moduleName
            ++ (case subModuleName of Just n -> "/" ++ n; Nothing -> "") ++ ".hs"
          string = headerF backend moduleName subModuleName ++ body

main :: IO ()
main = do
    putStrLn "futhask version 0.2.1"
    args <- getArgs
    [ backendS, jsonName, srcDir, moduleName ] <- getArgs >>= \args -> case args of
         [a, b, c, d] -> return args
         _            -> error "futhask takes four arguments:\n - backend (c, opencl, cuda)\n - Futhark JSON file\n - Haskell source directory\n - module name"
    backend <- case backendS of
        "c"      -> return C
        "opencl" -> return OpenCL
        "cuda"   -> return Cuda
        _        -> error $ "unknown backend: " ++ backendS ++ "\n  available backends: c, opencl, cuda"
    manifest <- readManifest jsonName
    -- putStrLn $ show header
    createDirectoryIfMissing False (srcDir ++ "/" ++ moduleName)
    mapM_ (writeModule backend srcDir moduleName)
        [ (Just "Raw", rawHeader, commonRawBody ++ (T.unpack . T.unlines $ rawImportString manifest))
        , (Just "Entries", entriesHeader, T.unpack . T.unlines $ entryCallLines manifest)
        , (Just "Types", typesHeader, T.unpack . T.unlines $ instanceDeclarationLines manifest)
        , (Just "TypeClasses", typeClassesHeader, typeClassesBody)
        , (Just "Context", contextHeader, contextBody)
        , (Just "Config", configHeader, configBody backend)
        , (Just "Fut", futHeader, futBody)
        , (Just "Wrap", wrapHeader, wrapBody)
        , (Just "Utils", utilsHeader, utilsBody)
        , (Nothing, exportsHeader, "") ]
