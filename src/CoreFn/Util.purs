-- |
-- Utilities for working with the JSON representation of the functional core.
--
module CoreFn.Util
  ( objectProp
  , objectProps
  , jsonIndex
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Array (head)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.StrMap as StrMap
import Data.Tuple (uncurry)
import Data.Unfoldable (class Unfoldable)

type Pair = { key :: String, value :: Json }

-- | Construct a `Pair` from the provided values.
pair :: String -> Json -> Pair
pair key value =
  { key, value }

-- |
-- Create an array of records by reading a JSON object.
--
objectProps
  :: forall f. Functor f => Unfoldable f => Json -> Either String (f Pair)
objectProps json = do
  object <- decodeJson json
  pure $ uncurry pair <$> StrMap.toUnfoldable object

-- |
-- Create a record by reading a JSON object.
--
-- The object is expected to consist of a single key-value pair, where the key
-- is unknown.
--
-- The provided error message is used if a key cannot be obtained.
--
objectProp :: String -> Json -> Either String Pair
objectProp message x = do
  pairs <- objectProps x
  note message $ head pairs

jsonIndex :: forall a. DecodeJson a => Json -> Int -> Either String a
jsonIndex json i = lmap (\err -> "[" <> show i <> "]" <> ": " <> err) do
  array :: Array Json <- decodeJson json
  value <- note "no value at index" $ Array.index array i
  decodeJson value
