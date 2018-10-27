module Core
    ( Program
    , Bind(..)
    , Variable(..)
    , Literal(..)
    , Expr(..)
    , CaseAlt(..)
    ) where

import Data.List (intercalate)
import Id
import Types

-- | A connection of an Id and a Type
data Variable =
    Variable Id
             Type
    deriving (Eq, Show)

-- | Literals
data Literal =
    LInt Int
    deriving (Eq, Show)

-- | Binding of a name to a variable
data Bind
    = NonRec Variable
             Expr
    | Rec [(Variable, Expr)]
    deriving (Eq, Show)

-- | Top Level Program
type Program = [Bind]

-- | The core lambda calculus
-- Lam is both Λ and λ, that is both types and vars.
data Expr
    = Var Id
    | Lit Literal
    | App Expr
          Expr
    | Lam Variable
          Expr
    | Let Bind
          Expr
    | Case Variable
           Expr
           [Alt]
    | Type Type
    deriving (Eq, Show)

-- | Case Alternatives
-- TODO: need to determine data constructor types
data CaseAlt
    = Constant Literal
    | Constructor [Variable]
    deriving (Eq, Show)

type Alt = (CaseAlt, Expr)
