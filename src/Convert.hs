module Convert

where

import Data.Text(Text(..), unpack)
import qualified Data.Map as M
import Manifest
import Type
import Debug

imap :: (Int -> a -> b) -> [a] -> [b]
imap f as = map (uncurry f) (zip [0..] as)

lookupType :: Manifest -> Text -> Type
lookupType manifest  = (M.!) (manifestTypes manifest)

mkFutharkType :: String -> Type -> FutharkType
mkFutharkType name ty =
  case ty of
    TypeArray  {} -> Array  { tyElem = readFutPrimType . unpack . tyArrayElemType $ ty
                            , tyRank = tyArrayRank     ty
                            }
    TypeOpaque {} -> Opaque { tyOpaqueBase = name
                            }

buildFutharkTypes :: Manifest -> M.Map String FutharkType
buildFutharkTypes manifest =
  let mapping = manifestTypes manifest
  in  M.mapWithKey mkFutharkType . M.mapKeys unpack $ mapping

lookupEntry :: Manifest -> Text -> EntryPoint
lookupEntry manifest = (M.!) (manifestEntryPoints manifest)

mkInParameter ::  M.Map String FutharkType -> Int -> Input -> FutharkParameter
mkInParameter mapping i input =
  FParam { pName   = "in" <> show i -- unpack . inputName $ input
         , pUnique = inputUnique input
         , pType   = mkFutType mapping . unpack . inputType $ input
         }

mkOutParameter :: M.Map String FutharkType -> Int -> Output -> FutharkParameter
mkOutParameter mapping i output =
  FParam { pName   = "out" <> show i
         , pUnique = outputUnique output
         , pType   = mkFutType mapping . unpack . outputType $ output
         }

mkEntry :: M.Map String FutharkType
        -> String
        -> EntryPoint
        -> FutharkEntry
mkEntry typeMap name entry =
  FutharkEntry
      { futEntryName = name
      , futEntryInParams  = imap (mkInParameter  typeMap) (entryPointInputs  entry)
      , futEntryOutParams = imap (mkOutParameter typeMap) (entryPointOutputs entry)
      }

buildEntries :: M.Map String FutharkType
             -> Manifest
             -> M.Map String FutharkEntry
buildEntries types manifest =
  let mapping = manifestEntryPoints manifest
  in  M.mapWithKey (mkEntry types) . M.mapKeys unpack $ mapping
