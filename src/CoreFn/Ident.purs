-- |
-- Names for value identifiers
--
module CoreFn.Ident
  ( Ident(..)
  , readIdent
  , readIdentJSON
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)
import Data.Generic (gShow, class Generic)
import Data.Maybe (Maybe)

data Ident
  -- |
  -- An alphanumeric identifier
  --
  = Ident String
  -- |
  -- A generated name for an identifier
  --
  | GenIdent (Maybe String) Int

derive instance eqIdent :: Eq Ident
derive instance genericIdent :: Generic Ident
derive instance ordIdent :: Ord Ident

instance showIdent :: Show Ident where
  show = gShow

instance decodeJsonIdent :: DecodeJson Ident where
  decodeJson = readIdent

readIdent :: Json -> Either String Ident
readIdent x = Ident <$> decodeJson x

readIdentJSON :: String -> Either String Ident
readIdentJSON = jsonParser >=> readIdent
