{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}

-- | C manifest data structure and serialisation to JSON.
--
-- A manifest contains machine-readable information about the API of
-- the compiled Futhark program.  Specifically which entry points are
-- available, which types are exposed, and what their C names are.
module Manifest
  ( Manifest (..)
  , Input (..)
  , Output (..)
  , EntryPoint (..)
  , Type (..)
  , ArrayOps (..)
  , OpaqueOps (..)
  )
where

import Data.Aeson (ToJSON (..), object)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON
import Data.Aeson ( Object(..)
                  , Value(..)
                  , pattern Object
                  , (.:)
                  )
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (bimap)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Data.HashMap.Lazy as HML ( lookup )

-- | Manifest info for an entry point parameter.
data Input = Input
  { inputName   :: T.Text
  , inputType   :: T.Text
  , inputUnique :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Manifest info for an entry point return value.
data Output = Output
  { outputType   :: T.Text
  , outputUnique :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Manifest info for an entry point.
data EntryPoint = EntryPoint
  { entryPointCFun    :: T.Text
  , entryPointOutputs :: [Output]
  , entryPointInputs  :: [Input]
  }
  deriving (Eq, Ord, Show)

-- | The names of the C functions implementing the operations on some
-- array type.
data ArrayOps = ArrayOps
  { arrayFree   :: T.Text
  , arrayShape  :: T.Text
  , arrayValues :: T.Text
  , arrayNew    :: T.Text
  }
  deriving (Eq, Ord, Show)

-- | The names of the C functions implementing the operations on some
-- opaque type.
data OpaqueOps = OpaqueOps
  { opaqueFree    :: T.Text
  , opaqueStore   :: T.Text
  , opaqueRestore :: T.Text
  }
  deriving (Eq, Ord, Show)

-- | Manifest info for a non-scalar type.  Scalar types are not part
-- of the manifest for a program.
data Type
  = -- | ctype, Futhark elemtype, rank.
    TypeArray
      { tyArrayCType    :: T.Text
      , tyArrayElemType :: T.Text
      , tyArrayRank     :: Int
      , tyArrayOps      :: ArrayOps
      }
  | TypeOpaque
      { tyOpaqueCType :: T.Text
      , tyOpaqueOps   :: OpaqueOps
      }
  deriving (Eq, Ord, Show)

-- | A manifest for a compiled program.
data Manifest = Manifest
  { -- | A mapping from Futhark entry points to how they are
    -- represented in C.
    manifestEntryPoints :: M.Map T.Text EntryPoint,
    -- | A mapping from Futhark type name to how they are represented
    -- at the C level.  Should not contain any of the primitive scalar
    -- types.  For array types, these have empty dimensions,
    -- e.g. @[]i32@.
    manifestTypes :: M.Map T.Text Type,
    -- | The compiler backend used to
    -- compile the program, e.g. @c@.
    manifestBackend :: T.Text
  }
  deriving (Eq, Ord, Show)

instance JSON.FromJSON ArrayOps where
  parseJSON (Object v) =
    ArrayOps <$> v .: "free"   --
             <*> v .: "shape"  --
             <*> v .: "values" --
             <*> v .: "new"    --

instance JSON.FromJSON OpaqueOps where
  parseJSON (Object v) =
    OpaqueOps <$> v .: "free"    --
              <*> v .: "store"   --
              <*> v .: "restore" --


instance JSON.FromJSON Manifest where
  parseJSON (Object v) =
    Manifest <$> v .: "backend"      --
             <*> v .: "entry_points" --
             <*> v .: "types"        --
-- entrypoints -- object $ map (bimap JSON.fromText onEntryPoint) $ M.toList entry_points
-- types object $ map (bimap JSON.fromText onType) $ M.toList types

instance JSON.FromJSON EntryPoint where
  parseJSON (Object v) =
    EntryPoint <$> v .: "cfun"    --
               <*> v .: "outputs" --
               <*> v .: "inputs"  --


instance JSON.FromJSON Output where
  parseJSON (Object v) =
    Output <$> v .: "type"   --
           <*> v .: "unique" --

instance JSON.FromJSON Input where
  parseJSON (Object v) =
    Input <$> v .: "name"   --
          <*> v .: "type"   --
          <*> v .: "unique" --

instance JSON.FromJSON Type where
  parseJSON = JSON.withObject "Type" $
    \ty -> do
      (kind :: T.Text) <- ty .: "kind"
      case (kind) of
        "array"  ->
          TypeArray <$> ty .: "ctype"    --
                    <*> ty .: "elemtype" --
                    <*> ty .: "rank"     --
                    <*> ty .: "ops"      --
        "opaque" ->
          TypeOpaque <$> ty .: "ctype"   --
                     <*> ty .: "ops"     --
