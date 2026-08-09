{-# LANGUAGE DeriveGeneric #-}

module Dataset.Empirical.IrisPCA2 
    ( IrisPCA2(..)
    , fromIrisPCA2
    ) where

import Data.Csv     (FromNamedRecord)
import GHC.Generics (Generic)

data IrisPCA2 = IrisPCA2
    { pc1   :: !Double
    , label :: !Double
    } deriving (Show, Generic)

instance FromNamedRecord IrisPCA2

fromIrisPCA2 :: IrisPCA2 -> (Double, Double)
fromIrisPCA2 irisRow = (pc1 irisRow, label irisRow)