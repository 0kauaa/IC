{-# LANGUAGE DeriveGeneric #-}

module Dataset.Empirical.IrisPCA2 
    ( IrisPCA2(..)
    , fromIris
    ) where

import Data.Csv     (FromNamedRecord)
import GHC.Generics (Generic)

data IrisPCA2 = IrisPCA2
    {
        pc1   :: !Double,
        label :: !Double
    } deriving (Show, Generic)

instance FromNamedRecord IrisPCA2

fromIris :: IrisPCA2 -> (Double, Double)
fromIris irisRow = (pc1 irisRow, label irisRow)