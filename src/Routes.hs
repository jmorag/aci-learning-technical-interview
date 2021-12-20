{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Routes where

import Control.Lens
import Course
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Time.Clock
import Data.Type.Map
import Data.Type.Set (Sort)
import Databass
import GHC.TypeLits
import Relude hiding (group)
import Servant

type CourseAPI =
  "courses"
    :> ( Get '[JSON] (Tuple '["courses" ::: [Tuple (Course :!! '["id", "name"])]])
          :<|> Capture "id" Int :> Get '[JSON] (Tuple (Sort (Rename "createdAt" "created_at" (Course :\ "deletedAt"))))
          :<|> ReqBody '[JSON] (Tuple (Course :!! '["name", "status"]))
            :> PostCreated '[JSON] (Headers '[Header "Location" Link] (Tuple Course))
          :<|> Capture "id" Int
            :> ReqBody '[JSON] (Tuple (Course :!! '["name", "status"]))
            :> Put '[JSON] (Tuple Course)
          :<|> Capture "id" Int :> DeleteNoContent
       )

courseAPI :: Proxy CourseAPI
courseAPI = Proxy

