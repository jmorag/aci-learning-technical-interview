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
import Network.Wai.Handler.Warp

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

type DB = IORef (MapDB Schema, Int)
type InnerMonad = ReaderT DB Handler

server :: ServerT CourseAPI InnerMonad
server = getCourses :<|> getCourse :<|> createCourse :<|> error "WIP"
  where
    getDB = readIORef =<< ask
    putDB (db', i') = do
      ref <- ask
      writeIORef ref (db', i')
    getCourses = do
      (db, i) <- getDB
      let query = table @"courses" @Schema & project @'["id", "name"]
      pure (Ext Var (runQuery db query) Empty)
    getCourse id_ = do
      (db, _) <- getDB
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
      (db, nextId) <- getDB
      case runQuery db (table @"courses" @Schema & restrict (\t -> t ^. col @"name" == name)) of
        [] -> do
          now <- liftIO getCurrentTime
          let course = nextId <| name <| status <| now <| now <| Nothing <| Empty
              db' = insert @"courses" @Schema (asMap @Course course) db
          putDB (db', nextId + 1)
          pure (addHeader (safeLink courseAPI (Proxy @("courses" :> GetCourse)) nextId) course)
        _ -> throwError $ err400 { errBody = "Course " <> show (unName name) <> " already exists" }

app :: DB -> Application
app dbRef = serve courseAPI (hoistServer courseAPI (nt dbRef) server)

nt :: DB -> InnerMonad a -> Handler a
nt dbRef reader = runReaderT reader dbRef

newApp :: IO Application
newApp = app <$> newIORef (initDB @Schema, 1)
