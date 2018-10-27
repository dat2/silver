module Id
    ( Id(..)
    ) where

import Data.String (IsString(..))

-- | Identifiers
newtype Id =
    Id String
    deriving (Eq, Show)

instance IsString Id where
    fromString = Id
