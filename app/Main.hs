module Main where

import System.IO
import Data.Maybe
import Data.List (intercalate)
import Debug.Trace
import System.Directory
import System.Environment
import CodeBodies
import Conversion
import Headers
import Backends

writeModule backend directory moduleName (subModuleName, headerF, body) 
    = writeFile fn string
    where fn = directory ++ "/" ++ moduleName 
            ++ (case subModuleName of Just n -> "/" ++ n; Nothing -> "") ++ ".hs"
          string = headerF backend moduleName subModuleName ++ body

main :: IO ()
main = do
    (backendS : headerName : srcDir : moduleName : _) <- getArgs
    backend <- case backendS of
        "c"      -> return C
        "opencl" -> return OpenCL
        "cuda"   -> return Cuda
        _        -> error $ "unknown backend: " ++ backendS
    header <- readHeader headerName
    
    createDirectoryIfMissing False (srcDir ++ "/" ++ moduleName)
    mapM_ (writeModule backend srcDir moduleName)
        [ (Just "Raw", rawHeader, rawImportString header)
        , (Just "Entries", entriesHeader, entryCallString header)
        , (Just "Types", typesHeader, instanceDeclarationString header)
        , (Just "TypeClasses", typeClassesHeader, typeClassesBody)
        , (Just "Context", contextHeader, contextBody)
        , (Just "Config", configHeader, configBody backend)
        , (Just "FT", fTHeader, fTBody)
        , (Just "Utils", utilsHeader,utilsBody)
        , (Nothing, exportsHeader, "") ]

