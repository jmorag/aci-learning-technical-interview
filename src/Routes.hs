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
import Network.Wai.Handler.Warp
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
    :> PostCreated '[JSON] (Headers '[Header "Location" Link] (Tuple (Course :\ "deletedAt")))

type UpdateCourse =
  Capture "id" Int
    :> ReqBody '[JSON] (Tuple (Course :!! '["name", "status"]))
    :> PutNoContent

type DeleteCourse = Capture "id" Int :> DeleteNoContent

courseAPI :: Proxy CourseAPI
courseAPI = Proxy

type DB = MVar (MapDB Schema, Int)

type InnerMonad = ReaderT DB Handler

server :: ServerT CourseAPI InnerMonad
server = getCourses :<|> getCourse :<|> createCourse :<|> updateCourse :<|> deleteCourse

getDB = takeMVar =<< ask

putDB (db', i') = do
  ref <- ask
  putMVar ref (db', i')

getCourses = do
  (db, nextId) <- getDB
  let query =
        table @"courses" @Schema
          & restrict (\t -> t ^. col @"deletedAt" & isNothing)
          & project @'["id", "name"]
  putDB (db, nextId)
  pure (Ext (Var @"courses") (runQuery db query) Empty)

getCourse id_ = do
  (db, nextId) <- getDB
  let query =
        table @"courses" @Schema
          & restrict (\t -> t ^. col @"id" == id_)
          & rename @"createdAt" @"created_at"
      results = runQuery db query
  putDB (db, nextId)
  case results of
    [] -> throwError $ err404 {errBody = "Course " <> show id_ <> " not found"}
    [r] -> case r ^. col @"deletedAt" of
      Nothing -> pure (submap r)
      Just _ -> throwError err410
    _ ->
      throwError $
        err500
          { errBody = "Internal server error, should only have gotten one result with id " <> show id_
          }

createCourse (Ext _ name (Ext _ status Empty)) = do
  (db, nextId) <- getDB
  let query =
        table @"courses" @Schema
          & restrict (\t -> (t ^. col @"deletedAt" & isNothing) && t ^. col @"name" == name)
  case runQuery db query of
    [] -> do
      now <- liftIO getCurrentTime
      let course = nextId <| name <| status <| now <| now <| Nothing <| Empty
          db' = insert @"courses" @Schema (asMap @Course course) db
      putDB (db', nextId + 1)
      pure (addHeader (safeLink courseAPI (Proxy @("courses" :> GetCourse)) nextId) (submap course))
    _ -> do
      putDB (db, nextId)
      throwError $ err400 {errBody = "Course " <> show (unName name) <> " already exists"}

updateCourse id_ (Ext _ name (Ext _ status Empty)) = do
  (db, nextId) <- getDB
  case runQuery db (table @"courses" @Schema & restrict (\t -> t ^. col @"id" == id_)) of
    [] -> throwError $ err404 {errBody = "Course " <> show id_ <> " not found"}
    [course]
      | isJust (course ^. col @"deletedAt") -> do
        putDB (db, nextId)
        throwError $ err410 {errBody = "Course deleted"}
      | runQuery
          db
          ( table @"courses" @Schema
              & restrict (\t -> t ^. col @"id" /= id_ && t ^. col @"name" == name)
          )
          & (not . null) -> do
        putDB (db, nextId)
        throwError $ err400 {errBody = "Course " <> show (unName name) <> " already exists"}
      | otherwise -> do
        now <- liftIO getCurrentTime
        let db' =
              updateTable @"courses" @Schema
                (\t -> t ^. col @"id" == id_)
                (\t -> t & col @"name" .~ name & col @"status" .~ status & col @"updatedAt" .~ now)
                db
        putDB (db', nextId)
        pure NoContent
    _ -> do
      putDB (db, nextId)
      throwError $
        err500
          { errBody = "Internal server error, should only have gotten one result with id " <> show id_
          }

deleteCourse id_ = do
  (db, nextId) <- getDB
  case runQuery db (table @"courses" @Schema & restrict (\t -> t ^. col @"id" == id_)) of
    [] -> do
      putDB (db, nextId)
      throwError $ err404 {errBody = "Course " <> show id_ <> " not found"}
    [course]
      | isJust (course ^. col @"deletedAt") -> do
        putDB (db, nextId)
        throwError $ err410 {errBody = "Course deleted"}
      | otherwise -> do
        now <- liftIO getCurrentTime
        let db' =
              updateTable @"courses" @Schema
                (\t -> t ^. col @"id" == id_)
                (\t -> t & col @"updatedAt" .~ now & col @"deletedAt" ?~ now)
                db
        putDB (db', nextId)
        pure NoContent
    _ -> do
      putDB (db, nextId)
      throwError $
        err500
          { errBody = "Internal server error, should only have gotten one result with id " <> show id_
          }

app :: DB -> Application
app dbRef = serve courseAPI (hoistServer courseAPI (nt dbRef) server)

nt :: DB -> InnerMonad a -> Handler a
nt dbRef reader = runReaderT reader dbRef

newApp :: IO Application
newApp = app <$> newMVar (initDB @Schema, 1)
