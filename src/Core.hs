{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Core
    ( Expr(..)
    , ExprF(..)
    , Bind(..)
    , Program
    , Id(..)
    , Literal(..)
    , Type(..)
    ) where

import Data.Functor.Foldable (Base, Recursive(..))
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List (intercalate)

-- Identifiers
newtype Id =
    Id String
    deriving (Eq)

instance Show Id where
    show (Id s) = s

-- | Literals
data Literal =
    LInt32 Int
    deriving (Eq)

instance Show Literal where
    show (LInt32 l) = show l

-- | Types
data Type
    = TyInt32
    | TyArrow Type
              Type
    deriving (Eq)

instance Show Type where
    show (TyInt32) = "Int32"
    show (TyArrow a b) = show a ++ " -> " ++ show b

-- | Binding of a name to a variable
data Bind
    = NonRecursive Id
                   Type
                   Expr
    | Recursive [(Id, Type, Expr)]
    deriving (Eq)

instance Show Bind where
    show (NonRecursive name ty e) = showBinding (name, ty, e)
    show (Recursive bs) = intercalate "\n" $ map showBinding bs

showBinding :: (Id, Type, Expr) -> String
showBinding (name, ty, e) = show name ++ " :: " ++ show ty ++ " = " ++ show e

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
    deriving (Eq)

makeBaseFunctor ''Expr

instance Show Expr where
    show = cata alg
      where
        alg :: Base Expr String -> String
        alg (VarF s) = show s
        alg (LitF i) = show i
        -- recursive cases
        alg (AppF l r) = "(" ++ l ++ " " ++ r ++ ")"
        alg (LamF v t b) =
            "(\\" ++ (show v) ++ " :: " ++ show t ++ " -> " ++ b ++ ")"
        alg (LetF b e) = "let " ++ show b ++ " in " ++ e

-- | Top Level Program
type Program = [Bind]
