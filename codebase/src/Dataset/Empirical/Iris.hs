{-# LANGUAGE DeriveGeneric #-}

module Dataset.Empirical.Iris
    ( Iris(..)
    , fromIris
    ) where

import Data.Csv     (FromNamedRecord)
import GHC.Generics (Generic)
import Core.Multi   (Multi(..))

data Iris = Iris
    { sepal_length :: !Double
    , sepal_width  :: !Double
    , petal_length :: !Double
    , petal_width  :: !Double
    , labels       :: !Int
    } deriving (Show, Generic)

instance FromNamedRecord Iris

fromIris :: Iris -> (Multi '[[Double]], Int)
fromIris irisRow =
    ( [ sepal_length irisRow
      , sepal_width  irisRow
      , petal_length irisRow
      , petal_width  irisRow
      ] :-:          MultiNull
      , labels       irisRow
    )
