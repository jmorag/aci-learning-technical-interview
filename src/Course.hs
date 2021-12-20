{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Course where

import Data.Aeson
import Data.Char (isSpace)
import qualified Data.Text as T
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

newtype Name = Name { unName :: Text}
  deriving (Show, Eq)

instance FromJSON Name where
  parseJSON = withText "Name" \t ->
    if T.null (T.filter (not . isSpace) t)
      then fail "Name must have at least one non-space character"
      else pure (Name t)

instance ToJSON Name where
  toJSON (Name t) = toJSON t

type Course =
  '[ "id" ::: Int,
     "name" ::: Name,
     "status" ::: Status,
     "createdAt" ::: UTCTime,
     "updatedAt" ::: UTCTime,
     "deletedAt" ::: Maybe UTCTime
   ]

type Schema = '["courses" ::: T Course '["id"]]
