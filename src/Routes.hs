{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
module Routes where

import Course
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Type.Map
import GHC.TypeLits
import Relude hiding (Map)
import Servant.API

type CourseAPI =
  "courses"
    :> Get
        '[JSON]
        (Map '["courses" ':-> [Map '["id" ':-> Int, "name" ':-> Text]]])
    :<|> "courses" :> Capture "id" Int
      :> Get
          '[JSON]
          ( Map
              '[ "id" ':-> Int,
                 "name" ':-> Text,
                 "status" ':-> Status,
                 "created_at" ':-> UTCTime,
                 "updatedAt" ':-> UTCTime
               ]
          )

exGetAllCourses :: Map '["courses" ':-> [Map '["id" ':-> Int, "name" ':-> Text]]]
exGetAllCourses = Ext Var ([Ext (Var @"id") 123 $ Ext (Var @"name") "Linux for Children" Empty]) Empty

