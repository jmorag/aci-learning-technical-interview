{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Routes where

import Control.Lens hiding (Empty, (<|))
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
    :> (GetCourses :<|> GetCourse :<|> CreateCourse :<|> UpdateCourse :<|> DeleteCourse)

type GetCourses = Get '[JSON] (Tuple '["courses" ::: [Tuple (Course :!! '["id", "name"])]])

type GetCourse =
  Capture "id" Int
    :> Get '[JSON] (Tuple (Sort (Rename "createdAt" "created_at" (Course :\ "deletedAt"))))

type CreateCourse =
  ReqBody '[JSON] (Tuple (Course :!! '["name", "status"]))
    :> PostCreated '[JSON] (Headers '[Header "Location" Link] (Tuple Course))

type UpdateCourse =
  Capture "id" Int
    :> ReqBody '[JSON] (Tuple (Course :!! '["name", "status"]))
    :> Put '[JSON] (Tuple Course)

type DeleteCourse = Capture "id" Int :> DeleteNoContent

courseAPI :: Proxy CourseAPI
courseAPI = Proxy

type InnerMonad = StateT (MapDB Schema, Int) Handler

server :: ServerT CourseAPI InnerMonad
server = getCourses :<|> getCourse :<|> createCourse :<|> error "WIP"
  where
    getCourses = do
      (db, _) <- get
      let query = table @"courses" @Schema & project @'["id", "name"]
      pure (Ext Var (runQuery db query) Empty)
    getCourse id_ = do
      (db, _) <- get
      let query =
            table @"courses" @Schema
              & restrict (\t -> t ^. col @"id" == id_)
              & rename @"createdAt" @"created_at"
          results = runQuery db query
      case results of
        [] -> throwError $ err404 {errBody = "id " <> show id_ <> " not found"}
        [r] -> case r ^. col @"deletedAt" of
          Nothing -> pure (submap r)
          Just _ -> throwError err410
        _ ->
          throwError $
            err500
              { errBody = "Internal server error, should only have gotten one result with id " <> show id_
              }
    createCourse (Ext _ name (Ext _ status Empty) :: Tuple '["name" ::: _, "status" ::: _]) = do
      (db, nextId) <- get
      now <- liftIO getCurrentTime
      let course = nextId <| name <| status <| now <| now <| Nothing <| Empty
          db' = insert @"courses" @Schema (asMap @Course course) db
      put (db', nextId + 1)
      pure (addHeader (safeLink courseAPI (Proxy @("courses" :> GetCourse)) nextId) course)

app :: Application
app = serve courseAPI (hoistServer courseAPI nt server)

nt :: InnerMonad a -> Handler a
nt st = evalStateT st (initDB @Schema, 1)
