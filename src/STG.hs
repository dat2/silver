module STG where

import Data.Int (Int32)

data Prog =
    Prog [Binding]

data Binding = Binding
    { bName :: String
    , bLf :: LambdaForm
    }

data LambdaForm = LambdaForm
    { lfFreeVars :: [Var]
    , lfUpdate :: UpdateFlag
    , lfArgs :: [Var]
    , lfExpr :: Expr
    }

data Var =
    Var String

data UpdateFlag
    = Updateable
    | NonUpdateable

data Expr
    = Let [Binding]
          Expr
    | LetRec [Binding]
             Expr
    | Case Expr
           [Alt]
           DefaultAlt
    | App Var
          [Atom]
    | Constructor [Atom]
    | PrimOp [Atom]
    | Lit Literal

data Alt
    = ConstructorAlt String
                     [Var]
                     Expr
    | Literal Expr

data DefaultAlt
    = NamedDefault Var
                   Expr
    | Default Var
              Expr

data Literal =
    LInt32 Int32

data PrimOp
    = Add
    | Sub
    | Mult
    | Div

data Atom
    = AVar Var
    | ALit Literal
