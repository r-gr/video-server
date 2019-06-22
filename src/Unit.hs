{-# LANGUAGE DeriveGeneric #-}

module Unit ( Unit(..)
            , UnitID
            , WireID
            ) where

import Data.Aeson (FromJSON)
import GHC.Generics

type WireID  = Int
type UnitID  = Int
data Unit    = Unit { unitName    :: String
                    , unitID      :: UnitID
                    , unitInputs  :: [WireID]
                    , unitOutputs :: [WireID]
                    } deriving (Generic, Show)

instance FromJSON Unit
