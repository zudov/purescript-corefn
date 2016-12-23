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
import Data.Foreign.Keys as K
import CoreFn.Ident (Ident(..))
import CoreFn.Names (Qualified, readQualified)
import CoreFn.Util (objectProp)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldMap)
import Data.Foreign (F, Foreign, ForeignError(..), fail, parseJSON, readArray, readBoolean, readChar, readInt, readNumber, readString)
import Data.Foreign.Class (readProp)
import Data.Foreign.Index (prop)
import Data.Monoid (mempty)
import Data.Traversable (class Traversable, foldlDefault, foldrDefault, intercalate, sequence, sequenceDefault, traverse)
import Data.Tuple (Tuple(..))

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

instance foldableLiteral :: Foldable Literal where
  foldMap f (ArrayLiteral x) = foldMap f x
  foldMap f (ObjectLiteral x) = foldMap (foldMap f) x
  foldMap _ _ = mempty

  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance functorLiteral :: Functor Literal where
  map _ (NumericLiteral x) = NumericLiteral x
  map _ (StringLiteral x) = StringLiteral x
  map _ (CharLiteral x) = CharLiteral x
  map _ (BooleanLiteral x) = BooleanLiteral x
  map f (ArrayLiteral x) = ArrayLiteral (map f x)
  map f (ObjectLiteral x) = ObjectLiteral (map (map f) x)

instance showLiteral :: Show a => Show (Literal a) where
  show (NumericLiteral e) = "(NumericLiteral " <> either show show e <> ")"
  show (StringLiteral s) = "(StringLiteral " <> show s <> ")"
  show (CharLiteral c) = "(CharLiteral " <> show c <> ")"
  show (BooleanLiteral b) = "(BooleanLiteral " <> show b <> ")"
  show (ArrayLiteral a) = "(ArrayLiteral " <> show a <> ")"
  show (ObjectLiteral o) = "(ObjectLiteral" <> show o <> ")"

instance traversableLiteral :: Traversable Literal where
  traverse _ (NumericLiteral x) = pure (NumericLiteral x)
  traverse _ (StringLiteral x) = pure (StringLiteral x)
  traverse _ (CharLiteral x) = pure (CharLiteral x)
  traverse _ (BooleanLiteral x) = pure (BooleanLiteral x)
  traverse f (ArrayLiteral x) = ArrayLiteral <$> traverse f x
  traverse f (ObjectLiteral x) = ObjectLiteral <$> traverse (traverse f) x

  sequence = sequenceDefault

readLiteral :: Foreign -> F (Literal (Expr Unit))
readLiteral x = do
  label <- readProp 0 x >>= readString
  readLiteral' label x

  where

  readValues :: Array Foreign -> F (Array (Expr Unit))
  readValues = traverse readExpr

  readPair :: Foreign -> String -> F (Tuple String (Expr Unit))
  readPair obj key = Tuple key <$> (prop key obj >>= readExpr)

  readPairs :: Foreign -> Array String -> F (Array (Tuple String (Expr Unit)))
  readPairs obj = sequence <<< (map <<< readPair) obj

  readLiteral' :: String -> Foreign -> F (Literal (Expr Unit))
  readLiteral' "IntLiteral" v = do
    value <- readProp 1 v
    NumericLiteral <$> Left <$> readInt value
  readLiteral' "NumberLiteral" v = do
    value <- readProp 1 v
    NumericLiteral <$> Right <$> readNumber value
  readLiteral' "StringLiteral" v = do
    value <- readProp 1 v
    StringLiteral <$> readString value
  readLiteral' "CharLiteral" v = do
    value <- readProp 1 v
    CharLiteral <$> readChar value
  readLiteral' "BooleanLiteral" v = do
    value <- readProp 1 v
    BooleanLiteral <$> readBoolean value
  readLiteral' "ArrayLiteral" v = do
    array <- readProp 1 v >>= readArray
    ArrayLiteral <$> readValues array
  readLiteral' "ObjectLiteral" v = do
    obj <- readProp 1 v
    keys <- K.keys obj
    ObjectLiteral <$> readPairs obj keys
  readLiteral' label _ = fail $ ForeignError $ "Unknown literal: " <> label

readLiteralJSON :: String -> F (Literal (Expr Unit))
readLiteralJSON json = parseJSON json >>= readLiteral

-- |
-- Data type for expressions and terms
--
data Expr a
  -- |
  -- A literal value
  --
  = Literal a (Literal (Expr a))
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

instance foldableExpr :: Foldable Expr where
  foldMap f (Literal m y) = f m <> foldMap (foldMap f) y
  foldMap f (App m x y) = f m <> foldMap f x <> foldMap f y
  foldMap f (Var x _) = f x

  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance functorExpr :: Functor Expr where
  map f (Literal x y) = Literal (f x) (map (map f) y)
  map f (App a x y) = App (f a) (map f x) (map f y)
  map f (Var x y) = Var (f x) y

instance showExpr :: Show a =>  Show (Expr a) where
  show (Literal x y) = "(Literal " <> show x <> " " <> show y <> ")"
  show (App x y z) = "(App " <> show x <> " " <> show y <> " " <> show z <> ")"
  show (Var x y) = "(Var " <> show x <> " " <> show y <> ")"

instance traversableExpr :: Traversable Expr where
  traverse f (Literal x y) = Literal <$> f x <*> traverse (traverse f) y
  traverse f (App a x y) = App <$> f a <*> traverse f x <*> traverse f y
  traverse f (Var x y) = Var <$> f x <*> pure y

  sequence = sequenceDefault

readExpr :: Foreign -> F (Expr Unit)
readExpr x = do
  label <- readProp 0 x >>= readString
  readExpr' label x

  where

  readExpr' :: String -> Foreign -> F (Expr Unit)
  readExpr' "Literal" y = do
    value <- readProp 1 y
    Literal unit <$> readLiteral value
  readExpr' "App" y = do
    expr1 <- readProp 1 y
    expr2 <- readProp 2 y
    App unit <$> readExpr expr1 <*> readExpr expr2
  readExpr' "Var" y = do
    value <- readProp 1 y
    Var unit <$> readQualified Ident value
  readExpr' label _ = fail $ ForeignError $ "Unknown expression: " <> label

readExprJSON :: String -> F (Expr Unit)
readExprJSON json = parseJSON json >>= readExpr

-- |
--  A let or module binding.
--
data Bind a
  -- |
  -- Non-recursive binding for a single value
  --
  = NonRec a Ident (Expr a)
  -- |
  -- Mutually recursive binding group for several values
  -- |
  | Rec (Array (Tuple (Tuple a Ident) (Expr a)))

derive instance eqBind :: Eq a => Eq (Bind a)
derive instance ordBind :: Ord a => Ord (Bind a)

instance showBind :: Show a => Show (Bind a) where
  show (NonRec x y z) = "(NonRec " <> show x <> show y <> show z <> ")"
  show (Rec x) = "(Rec " <> intercalate ", " (show <$> x) <> ")"

readBind :: Foreign -> F (Bind Unit)
readBind x = do
  o <- objectProp "Binding name not found" x
  -- | TODO: mutally recursive bindings
  expr <- readExpr o.value
  pure $ NonRec unit (Ident o.key) expr

readBindJSON :: String -> F (Bind Unit)
readBindJSON json = parseJSON json >>= readBind
