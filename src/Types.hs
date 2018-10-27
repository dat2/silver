{-# LANGUAGE OverloadedStrings #-}

module Types
    ( Type(..)
    , Kind(..)
    , tyInt32
    , tyArrow
    , tyFn
    ) where

import Id

-- | Kinds
data Kind
    = Star
    | KFun Kind
           Kind
    deriving (Eq, Show)

-- | TypeConstants
data TyCon =
    TyCon Id
          Kind
    deriving (Eq, Show)

-- | Type Variables
data TyVar =
    TyVar Id
          Kind
    deriving (Eq, Show)

-- | Types
data Type
    = TCon TyCon
    | TVariable TyVar
    | TApp Type
           Type
    deriving (Eq, Show)

tyInt32 :: Type
tyInt32 = TCon $ TyCon "Int32" Star

tyArrow :: Type
tyArrow = TCon $ TyCon "(->)" (KFun Star Star)

tyFn :: Type -> Type -> Type
a `tyFn` b = TApp (TApp tyArrow a) b

infixr 4 `tyFn`
