{-# LANGUAGE OverloadedStrings #-}

module Types
    ( Type(..)
    , Kind(..)
    , int32
    , arrow
    , fn
    , tvar
    ) where

import Name

-- | Kinds
data Kind
    = Star
    | KFun Kind
           Kind
    deriving (Eq, Show)

-- | TypeConstants
data TyCon =
    TyCon Name
          Kind
    deriving (Eq, Show)

-- | Type Variables
data TyVar =
    TyVar Name
          Kind
    deriving (Eq, Show)

-- | Types
data Type
    = TCon TyCon
    | TVariable TyVar
    | TApp Type
           Type
    deriving (Eq, Show)

int32 :: Type
int32 = TCon $ TyCon "Int32" Star

arrow :: Type
arrow = TCon $ TyCon "(->)" (KFun Star Star)

fn :: Type -> Type -> Type
a `fn` b = TApp (TApp arrow a) b

tvar :: Name -> TyVar
tvar i = TyVar i Star

infixr 4 `fn`
