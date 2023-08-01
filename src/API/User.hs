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
    :<|> Capture "id" Int :> Get '[JSON] (Maybe User)
  )

server :: Server API
server = getEntities :<|> findEntity
  where
    getEntities :: Maybe Int -> Maybe Int -> Handler [User]
    getEntities limit offset = liftIO . withDatabase
      $ \conn -> dbGetUsers conn 
        (M.fromMaybe 10 limit)
        (M.fromMaybe 0 offset)

    findEntity :: Int -> Handler (Maybe User)
    findEntity id_ = liftIO . withDatabase
      $ \conn -> dbFindUser conn id_

