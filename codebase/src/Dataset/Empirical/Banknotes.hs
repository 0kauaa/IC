{-# LANGUAGE DeriveGeneric #-}

module Dataset.Empirical.Banknotes
    ( Banknotes(..)
    , fromBanknotes
    ) where

import Data.Csv     (FromNamedRecord)
import GHC.Generics (Generic)
import Core.Multi   (Multi(..))

data Banknotes = Banknote
    { variance :: !Double
    , skewness :: !Double
    , curtosis :: !Double
    , entropy  :: !Double
    , label    :: !Double
    } deriving (Show, Generic)

instance FromNamedRecord Banknotes

fromBanknotes :: Banknotes -> (Multi '[[Double]], Double)
fromBanknotes bankRow =
    ( [ variance bankRow
      , skewness bankRow
      , curtosis bankRow
      , entropy  bankRow
      ] :-:      MultiNull
      , label    bankRow
    )