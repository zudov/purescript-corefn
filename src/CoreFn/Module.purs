module CoreFn.Module
  ( Module(..)
  , readModule
  , readModuleJSON
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, getField)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)

import CoreFn.Expr (Bind)
import CoreFn.Ident (Ident)
import CoreFn.Names (ModuleName(..))
import CoreFn.Util (objectProp)

-- |
-- The CoreFn module representation
--
data Module a = Module
  { builtWith :: String
  , moduleDecls :: Array (Bind a)
  , moduleExports :: Array Ident
  , moduleForeign :: Array Ident
  , moduleImports :: Array ModuleName
  , moduleName :: ModuleName
  }

derive instance eqModule :: Eq a => Eq (Module a)
derive instance ordModule :: Ord a => Ord (Module a)

instance showModule :: Show a => Show (Module a) where
  show (Module { builtWith
               , moduleDecls
               , moduleExports
               , moduleForeign
               , moduleImports
               , moduleName }) =
       "(Module { builtWith: " <> show builtWith <>
               ", moduleName: " <> show moduleName <>
               ", moduleDecls: " <> show moduleDecls <>
               ", moduleExports: " <> show moduleExports <>
               ", moduleForeign: " <> show moduleForeign <>
               ", moduleImports: " <> show moduleImports <>
               "})"

readModule :: Json -> Either String (Module Unit)
readModule x = do
  o <- objectProp "Module name not found" x
  v <- decodeJson o.value
  builtWith     <- getField v "builtWith"
  moduleDecls   <- getField v "decls"
  moduleExports <- getField v "exports"
  moduleForeign <- getField v "foreign"
  moduleImports <- getField v "imports"

  let moduleName = ModuleName o.key

  pure $ Module
    { builtWith
    , moduleDecls
    , moduleExports
    , moduleForeign
    , moduleImports
    , moduleName
    }

readModuleJSON :: String -> Either String (Module Unit)
readModuleJSON = jsonParser >=> readModule
