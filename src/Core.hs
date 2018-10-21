{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Core
    ( Program
    , Bind(..)
    , letn
    , letrec
    , Expr(..)
    , ExprF(..)
    , Id(..)
    , Literal(..)
    , Type(..)
    , tyInt32
    ) where

import Data.Functor.Foldable (Base, Recursive(..))
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List (intercalate)

-- Identifiers
newtype Id =
    Id String
    deriving (Eq, Show)

-- | Literals
data Literal =
    LInt Int
    deriving (Eq, Show)

-- | Types
data Type
    = TyCon Id
    | Type :-> Type
    deriving (Eq, Show)

tyInt32 :: Type
tyInt32 = TyCon (Id "Int32")

-- | Binding of a name to a variable
data Bind
    = NonRecursive Id
                   Expr
    | Recursive [(Id, Expr)]
    deriving (Eq, Show)

letn :: String -> Expr -> Bind
letn s = NonRecursive (Id s)

letrec :: [(String, Expr)] -> Bind
letrec = Recursive . map f
  where
    f (s, e) = (Id s, e)

-- | Case Alternatives
data AltKind
    = Constant Literal
    | Constructor [Id]
    deriving (Eq, Show)

type Alt = (AltKind, Expr)

-- | The core lambda calculus
data Expr
    = Var Id
    | Lit Literal
    | App Expr
          Expr
    | Lam Id
          Type
          Expr
    | Let Bind
          Expr
    | Case Expr
           [Alt]
    deriving (Eq, Show)

makeBaseFunctor ''Expr

-- | Top Level Program
type Program = [Bind]
