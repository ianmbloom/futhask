module Encode

where

import Control.Monad
import Data.List
import GHC.SourceGen
import GHC.Paths (libdir)
import GHC (runGhc, getSessionDynFlags)
import GHC.Utils.Outputable(Outputable(..))

import Type
import Declarations
import Generate

encodeGHC :: Outputable a => a ->  IO String
encodeGHC x = runGhc (Just libdir) $ do
   dynFlags <- getSessionDynFlags
   return $ showPpr dynFlags x

foreignDataWrappers :: [FutharkType] -> IO String
foreignDataWrappers types =
  unlines <$>
  mapM (encodeGHC . foreignDataDeclaration) types

foreignTypeOpDeclarations :: [FutharkType] -> IO String
foreignTypeOpDeclarations types =
  unlines . map (drop 2) <$>
  mapM encodeGHC (concatMap foreignTypeOpDeclarationSet types)

foreignEntryDeclarations :: [FutharkEntry] -> IO String
foreignEntryDeclarations entries =
  unlines . map (drop 2) <$>
  mapM encodeGHC (map foreignEntryDeclaration entries)

haskellDataWrappers :: [FutharkType] -> IO String
haskellDataWrappers types =
  unlines <$>
  mapM encodeGHC ( map dataDeclaration types)

haskellInstanceDeclarations :: [FutharkType] -> IO String
haskellInstanceDeclarations types =
  unlines <$>
  mapM encodeGHC (concatMap instanceDeclaration types)

haskellEntryDeclarations :: [FutharkEntry] -> IO String
haskellEntryDeclarations entries =
   unlines <$>
   mapM encodeGHC (concatMap declareEntry entries)
