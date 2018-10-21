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
                   Type
                   Expr
    | Recursive [(Id, Type, Expr)]
    deriving (Eq, Show)

letn :: String -> Type -> Expr -> Bind
letn s = NonRecursive (Id s)

letrec :: [(String, Type, Expr)] -> Bind
letrec = Recursive . map f
  where
    f (s, t, e) = (Id s, t, e)

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
    deriving (Eq, Show)

makeBaseFunctor ''Expr

-- | Top Level Program
type Program = [Bind]
