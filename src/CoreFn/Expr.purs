-- |
-- The core functional representation
--
module CoreFn.Expr
  ( Bind(..)
  , Expr(..)
  , Literal(..)
  , readBind
  , readBindJSON
  , readExpr
  , readExprJSON
  , readLiteral
  , readLiteralJSON
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.StrMap as StrMap
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import CoreFn.Ident (Ident(..))
import CoreFn.Names (Qualified, readQualified)
import CoreFn.Util (objectProps, jsonIndex)

-- |
-- Data type for literal values. Parameterised so it can be used for Exprs and
-- Binders.
--
data Literal a
  -- |
  -- A numeric literal
  --
  = NumericLiteral (Either Int Number)
  -- |
  -- A string literal
  --
  | StringLiteral String
  -- |
  -- A character literal
  --
  | CharLiteral Char
  -- |
  -- A boolean literal
  --
  | BooleanLiteral Boolean
  -- |
  -- An array literal
  --
  | ArrayLiteral (Array a)
  -- |
  -- An object literal
  --
  | ObjectLiteral (Array (Tuple String a))

derive instance eqLiteral :: Eq a => Eq (Literal a)
derive instance ordLiteral :: Ord a => Ord (Literal a)

instance showLiteral :: Show a => Show (Literal a) where
  show (NumericLiteral e) = "(NumericLiteral " <> either show show e <> ")"
  show (StringLiteral s) = "(StringLiteral " <> show s <> ")"
  show (CharLiteral c) = "(CharLiteral " <> show c <> ")"
  show (BooleanLiteral b) = "(BooleanLiteral " <> show b <> ")"
  show (ArrayLiteral a) = "(ArrayLiteral " <> show a <> ")"
  show (ObjectLiteral o) = "(ObjectLiteral" <> show o <> ")"

instance decodeJsonLiteral :: DecodeJson (Literal (Expr Unit)) where
  decodeJson = decodeJson >=> readLiteral

readLiteral :: Json -> Either String (Literal (Expr Unit))
readLiteral json = lmap ("Literal: " <> _) do
  label :: String <- arg0
  lmap (\err -> label <> ": " <> err) do
    value label
  where

  value :: String -> Either String (Literal (Expr Unit))
  value = case _ of
    "IntLiteral"     -> arg1 <#> Left  >>> NumericLiteral
    "NumberLiteral"  -> arg1 <#> Right >>> NumericLiteral
    "StringLiteral"  -> arg1 <#> StringLiteral
    "CharLiteral"    -> arg1 <#> CharLiteral
    "BooleanLiteral" -> arg1 <#> BooleanLiteral
    "ArrayLiteral"   -> arg1 <#> ArrayLiteral
    "ObjectLiteral"  -> arg1 <#> StrMap.toUnfoldable >>> ObjectLiteral
    _ -> Left "Unknown literal"

  arg0 :: ∀ a. DecodeJson a => Either String a
  arg0 = jsonIndex json 0

  arg1 :: ∀ a. DecodeJson a => Either String a
  arg1 = jsonIndex json 1

readLiteralJSON :: String -> Either String (Literal (Expr Unit))
readLiteralJSON = jsonParser >=> decodeJson

-- |
-- Data type for expressions and terms
--
data Expr a
  -- |
  -- A literal value
  --
  = Literal a (Literal (Expr a))
  -- |
  -- Function introduction
  --
  | Abs a Ident (Expr a)
  -- |
  -- Function application
  --
  | App a (Expr a) (Expr a)
  -- |
  -- Variable
  --
  | Var a (Qualified Ident)

derive instance eqExpr :: Eq a => Eq (Expr a)
derive instance ordExpr :: Ord a => Ord (Expr a)

instance showExpr :: Show a => Show (Expr a) where
  show (Literal x y) = "(Literal " <> show x <> " " <> show y <> ")"
  show (Abs x y z) = "(Abs " <> show x <> " " <> show y <> " " <> show z <> ")"
  show (App x y z) = "(App " <> show x <> " " <> show y <> " " <> show z <> ")"
  show (Var x y) = "(Var " <> show x <> " " <> show y <> ")"

instance decodeJsonExpr :: DecodeJson (Expr Unit) where
  decodeJson = readExpr

readExpr :: Json -> Either String (Expr Unit)
readExpr json = lmap ("Expr: " <> _) do
  label <- arg0
  lmap (\err -> label <> ": " <> err) do
    expr label
  where

  expr :: String -> Either String (Expr Unit)
  expr = case _ of
    "Literal" -> do
      value :: Literal (Expr Unit) <- arg1
      pure $ Literal unit value
    "Abs" -> do
      param :: Ident     <- lmap ("param: " <> _) arg1
      body  :: Expr Unit <- lmap ("body: " <> _)  arg2
      pure $ Abs unit param body
    "App" -> do
      expr1 :: Expr Unit <- lmap ("expr1: " <> _) arg1
      expr2 :: Expr Unit <- lmap ("expr2: " <> _) arg2
      pure $ App unit expr1 expr2
    "Var" -> do
      name :: Qualified Ident <- arg1
      pure $ Var unit name
    _ ->
      Left $ "Unknown expression"

  arg0 :: forall a. DecodeJson a => Either String a
  arg0 = jsonIndex json 0

  arg1 :: forall a. DecodeJson a => Either String a
  arg1 = jsonIndex json 1

  arg2 :: forall a. DecodeJson a => Either String a
  arg2 = jsonIndex json 2

readExprJSON :: String -> Either String (Expr Unit)
readExprJSON = jsonParser >=> readExpr

-- |
--  A let or module binding.
--
data Bind a = Bind (Array (Tuple (Tuple a Ident) (Expr a)))

derive instance eqBind :: Eq a => Eq (Bind a)
derive instance ordBind :: Ord a => Ord (Bind a)

instance showBind :: Show a => Show (Bind a) where
  show (Bind x) = "(Bind " <> show x <> ")"

instance decodeJsonBind :: DecodeJson (Bind Unit) where
  decodeJson = readBind

readBind :: Json -> Either String (Bind Unit)
readBind x = do
  pairs <- objectProps x
  bindings <- traverse fromPair pairs
  pure $ Bind bindings

  where

  fromPair
    :: { key :: String, value :: Json }
    -> Either String (Tuple (Tuple Unit Ident) (Expr Unit))
  fromPair pair = do
    expr <- readExpr pair.value
    let ident = Ident pair.key
    pure $ Tuple (Tuple unit ident) expr

readBindJSON :: String -> Either String (Bind Unit)
readBindJSON = jsonParser >=> readBind
