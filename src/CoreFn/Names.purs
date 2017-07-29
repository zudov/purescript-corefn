module CoreFn.Names
  ( ModuleName(..)
  , OpName(..)
  , ProperName(..)
  , Qualified(..)
  , readModuleName
  , readModuleNameJSON
  , readOpName
  , readOpNameJSON
  , readProperName
  , readProperNameJSON
  , readQualified
  , readQualifiedJSON
  ) where

import Prelude
import Data.Argonaut.Core as Json
import Data.Array as Array
import Control.MonadZero (guard)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), joinWith, split)

-- |
-- Module names
--
newtype ModuleName = ModuleName String

derive instance eqModuleName :: Eq ModuleName
derive instance genericModuleName :: Generic ModuleName
derive instance newtypeModuleName :: Newtype ModuleName _
derive instance ordModuleName :: Ord ModuleName
derive newtype instance showModuleName :: Show ModuleName

instance decodeJsonModuleName :: DecodeJson ModuleName where
  decodeJson = readModuleName

readModuleName :: Json -> Either String ModuleName
readModuleName x = ModuleName <$> decodeJson x

readModuleNameJSON :: String -> Either String ModuleName
readModuleNameJSON = jsonParser >=> readModuleName

-- |
-- Operator alias names.
--
newtype OpName = OpName String

derive instance eqOpName :: Eq OpName
derive instance genericOpName :: Generic OpName
derive instance newtypeOpName :: Newtype OpName _
derive instance ordOpName :: Ord OpName
derive newtype instance showOpName :: Show OpName

instance decodeJsonOpName :: DecodeJson OpName where
  decodeJson = readOpName

readOpName :: Json -> Either String OpName
readOpName x = OpName <$> decodeJson x

readOpNameJSON :: String -> Either String OpName
readOpNameJSON = jsonParser >=> readOpName

-- |
-- Proper name, i.e. capitalized names for e.g. module names, type/data
-- constructors.
--
newtype ProperName = ProperName String

derive instance eqProperName :: Eq ProperName
derive instance genericProperName :: Generic ProperName
derive instance newtypeProperName :: Newtype ProperName _
derive instance ordProperName :: Ord ProperName
derive newtype instance showProperName :: Show ProperName

instance decodeJsonProperName :: DecodeJson ProperName where
  decodeJson = readProperName

readProperName :: Json -> Either String ProperName
readProperName x = ProperName <$> decodeJson x

readProperNameJSON :: String -> Either String ProperName
readProperNameJSON = jsonParser >=> readProperName

-- |
-- A qualified name, i.e. a name with an optional module name
--
data Qualified a = Qualified (Maybe ModuleName) a

derive instance eqQualified :: (Generic a, Eq a) => Eq (Qualified a)
derive instance genericQualified :: (Generic a) => Generic (Qualified a)
derive instance ordQualified :: (Generic a, Ord a) => Ord (Qualified a)
derive instance functorQualified :: Functor Qualified

instance showQualified :: (Generic a, Show a) => Show (Qualified a) where
  show = gShow

instance decodeJsonQualified :: DecodeJson a => DecodeJson (Qualified a) where
  decodeJson = readQualified


readQualified
  :: forall a. DecodeJson a => Json -> Either String (Qualified a)
readQualified json = lmap ("Qualified: " <> _) do
  parseName =<< decodeJson json
  where

  parseName :: String -> Either String (Qualified a)
  parseName s = do
    let parts = split (Pattern delimiter) s
    { init, last } <- note "Empty name" $ Array.unsnoc parts
    let moduleName = toModuleName init
    valueName <- decodeJson $ Json.fromString last
    pure $ Qualified moduleName valueName

  toModuleName :: Array String -> Maybe ModuleName
  toModuleName parts = do
    guard $ not $ Array.null parts
    pure $ ModuleName $ joinWith delimiter parts

  delimiter = "."


readQualifiedJSON
  :: forall a. DecodeJson a => String -> Either String (Qualified a)
readQualifiedJSON = jsonParser >=> readQualified
