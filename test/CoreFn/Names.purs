module Test.CoreFn.Names
  ( testNames
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.Ident (Ident(..))
import CoreFn.Names (ModuleName(..), OpName(..), ProperName(..), Qualified(..), readModuleNameJSON, readOpNameJSON, readProperNameJSON, readQualifiedJSON)
import Data.Maybe (Maybe(..))
import Test.Util (assertEqual, expectSuccess)

testNames :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
testNames = do
  log ""
  log "Test Names"

  testModuleName
  testOpName
  testProperName
  testQualifiedIdentWithoutModuleName
  testQualifiedIdentWithModuleName
  testQualifiedOpNameWithoutModuleName
  testQualifiedOpNameWithModuleName
  testQualifiedProperNameWithoutModuleName
  testQualifiedProperNameWithModuleName

  where

  -- |
  -- ModuleName
  --
  testModuleName = do
    let description = "ModuleName from JSON results in success"

    let json = """
      "Main"
    """

    expectSuccess description (readModuleNameJSON json) \x ->
      assertEqual x (ModuleName "Main")

  -- |
  -- OpName
  --
  testOpName = do
    let description = "OpName from JSON results in success"

    let json = """
      "Control.Bind.bind"
    """

    expectSuccess description (readOpNameJSON json) \x ->
      assertEqual x (OpName "Control.Bind.bind")

  -- |
  -- ProperName
  --
  testProperName = do
    let description = "ProperName from JSON results in success"

    let json = """
      "Nothing"
    """

    expectSuccess description (readProperNameJSON json) \x ->
      assertEqual x (ProperName "Nothing")

  -- |
  -- Qualified
  --
  testQualifiedIdentWithoutModuleName = do
    let description = "Qualified Ident without module name from JSON results in success"

    let json = """
      "log"
    """

    expectSuccess description (readQualifiedJSON Ident json) \(Qualified x y) -> do
      assertEqual x Nothing
      assertEqual y (Ident "log")

  testQualifiedIdentWithModuleName = do
    let description = "Qualified Ident with module name from JSON results in success"

    let json = """
      "Control.Monad.Console.Eff.log"
    """

    expectSuccess description (readQualifiedJSON Ident json) \(Qualified x y) -> do
      assertEqual x (Just $ ModuleName "Control.Monad.Console.Eff")
      assertEqual y (Ident "log")

  testQualifiedOpNameWithoutModuleName = do
    let description = "Qualified OpName without module name from JSON results in success"

    let json = """
      "bind"
    """

    expectSuccess description (readQualifiedJSON OpName json) \(Qualified x y) -> do
      assertEqual x Nothing
      assertEqual y (OpName "bind")

  testQualifiedOpNameWithModuleName = do
    let description = "Qualified OpName with module name from JSON results in success"

    let json = """
      "Control.Bind.bind"
    """

    expectSuccess description (readQualifiedJSON OpName json) \(Qualified x y) -> do
      assertEqual x (Just $ ModuleName "Control.Bind")
      assertEqual y (OpName "bind")

  testQualifiedProperNameWithoutModuleName = do
    let description = "Qualified ProperName without module name from JSON results in success"

    let json = """
      "Nothing"
    """

    expectSuccess description (readQualifiedJSON ProperName json) \(Qualified x y) -> do
      assertEqual x Nothing
      assertEqual y (ProperName "Nothing")

  testQualifiedProperNameWithModuleName = do
    let description = "Qualified ProperName with module name from JSON results in success"

    let json = """
      "Data.Maybe.Nothing"
    """

    expectSuccess description (readQualifiedJSON ProperName json) \(Qualified x y) -> do
      assertEqual x (Just $ ModuleName "Data.Maybe")
      assertEqual y (ProperName "Nothing")
