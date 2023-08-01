{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API.User
  ( API
  , server
  ) where

import           Control.Monad.IO.Class
import           Model.User
import           Servant
import           Database
import           Database.SQL.User
import qualified Data.Maybe             as M

type API =
  (
    QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [User]
    :<|> ReqBody '[JSON] User :> Post '[JSON] User
    :<|> Capture "id" Int :> Get '[JSON] (Maybe User)
    :<|> Capture "id" Int :> ReqBody '[JSON] User :> Put '[JSON] (Maybe User)
    :<|> Capture "id" Int :> DeleteNoContent
  )

server :: Server API
server = getEntities
  :<|> createEntity
  :<|> findEntity
  :<|> replaceEntity
  :<|> deleteEntity
  where
    getEntities :: Maybe Int -> Maybe Int -> Handler [User]
    getEntities limit offset = liftIO . withDatabase
      $ \conn -> dbGetUsers conn
        (M.fromMaybe 10 limit)
        (M.fromMaybe 0 offset)
    
    createEntity :: User -> Handler User
    createEntity entity = liftIO . withDatabase
      $ \conn -> dbAddUser conn entity

    findEntity :: Int -> Handler (Maybe User)
    findEntity id_ = liftIO . withDatabase
      $ \conn -> dbFindUser conn id_

    replaceEntity :: Int -> User -> Handler (Maybe User)
    replaceEntity id_ entity = liftIO . withDatabase
      $ \conn -> dbReplaceUser conn id_ entity

    deleteEntity :: Int -> Handler NoContent
    deleteEntity id_ = liftIO . withDatabase
      $ \conn -> NoContent <$ dbDeleteUser conn id_
