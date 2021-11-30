{-# LANGUAGE OverloadedStrings #-}

module Name

where

import Data.Maybe
import Data.List (intercalate, partition, lookup)
import Data.Char (toUpper)
import qualified Data.Text as T
import Data.Text (Text(..))

primTable :: [(Text, Text)]
primTable =
    [ ("f32", "Float" )
    , ("f64", "Double")
    , ("bool","CBool" )
    , ("i8" , "Int8"  )
    , ("i16", "Int16" )
    , ("i32", "Int32" )
    , ("i64", "Int64" )
    , ("u8" , "Word8" )
    , ("u16", "Word16")
    , ("u32", "Word32")
    , ("u64", "Word64")
    ]

maybePrim :: Text -> Maybe Text
maybePrim key = lookup key primTable

fromRaw :: (Text -> Text) -> Text -> Text
fromRaw f key =
  case maybePrim key of
    Just prim -> f key
    Nothing   -> "futark_" <> f key

capitalize :: Text -> Text
capitalize text = T.singleton (toUpper (T.head text)) <> T.tail text
