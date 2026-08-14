{-# LANGUAGE DeriveGeneric #-}

module Dataset.Empirical.Iris2
    ( Iris2(..)
    , fromIris2
    ) where

import Data.Csv     (FromNamedRecord)
import GHC.Generics (Generic)
import Core.Multi   (Multi(..))

data Iris2 = Iris2
    { sepal_length :: !Double
    , sepal_width  :: !Double
    , petal_length :: !Double
    , petal_width  :: !Double
    , label        :: !Double
    } deriving (Show, Generic)

instance FromNamedRecord Iris2

fromIris2 :: Iris2 -> (Multi '[[Double]], Double)
fromIris2 irisRow =
    ( [ sepal_length irisRow
      , sepal_width  irisRow
      , petal_length irisRow
      , petal_width  irisRow
      ] :-:          MultiNull
      , label        irisRow
    )