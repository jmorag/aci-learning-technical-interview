{-# OPTIONS_GHC -fplugin=Evoke #-}
module Course where

import Data.Time.Clock
import Data.Aeson
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Relude

data Status = Scheduled | InProduction | Available
  deriving (Show, Eq)
