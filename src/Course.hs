{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Course where

import Data.Aeson
import Data.Time.Clock
import Databass
import Relude

data Status = Scheduled | InProduction | Available
  deriving (Show, Eq)

instance ToJSON Status where
  toJSON = \case
    Scheduled -> "scheduled"
    InProduction -> "in_production"
    Available -> "available"

instance FromJSON Status where
  parseJSON = withText "Status" \case
    "scheduled" -> pure Scheduled
    "in_production" -> pure InProduction
    "available" -> pure Available
    other -> fail "expected one of 'scheduled', 'in_production', or 'available'"

type Course =
    '[ "id" ::: Int,
       "name" ::: Text,
       "status" ::: Status,
       "createdAt" ::: UTCTime,
       "updatedAt" ::: UTCTime,
       "deletedAt" ::: Maybe UTCTime
     ]

type Schema = '["courses" ::: T Course '["id"]]
