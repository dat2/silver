module Name
    ( Name(..)
    ) where

import Data.String (IsString(..))

-- | Nameentifiers
newtype Name =
    Name String
    deriving (Eq, Show)

instance IsString Name where
    fromString = Name
