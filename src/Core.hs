module Core
    ( Program
    , Bind(..)
    , Var(..)
    , Literal(..)
    , Expr(..)
    , CaseAlt(..)
    ) where

import Data.List (intercalate)
import Name
import Types

-- | A connection of an Name and a Type
data Var = Id
    { varName :: Name
    , varType :: Type
    } deriving (Eq, Show)

-- | Literals
data Literal =
    LitInt Int
    deriving (Eq, Show)

-- | Binding of a name to a variable
data Bind
    = NonRec Var
             Expr
    | Rec [(Var, Expr)]
    deriving (Eq, Show)

-- | Top Level Program
type Program = [Bind]

-- | The core lambda calculus
-- Lam is both Λ and λ, that is both types and vars.
data Expr
    = Var Name
    | Lit Literal
    | App Expr
          Expr
    | Lam Var
          Expr
    | Let Bind
          Expr
    | Case Var
           Expr
           [Alt]
    | Type Type
    deriving (Eq, Show)

-- | Case Alternatives
-- TODO: need to determine data constructor types
data CaseAlt
    = Constant Literal
    | Constructor [Var]
    deriving (Eq, Show)

type Alt = (CaseAlt, Expr)
