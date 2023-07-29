{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API.User (
  API
  , server
  ) where

import           Control.Monad.IO.Class
import           Data.Maybe
import           Database.SQLite.Simple
import           Model.User
import           Servant

type API
  = QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [User]
  :<|> Capture "id" Int :> Get '[JSON] (Maybe User)

server :: Server API
server = getEntities :<|> findEntity
  where
    getEntities :: Maybe Int -> Maybe Int -> Handler [User]
    getEntities limit offset = liftIO $ withConnection "./sqlite.db" ff
      where
        ff conn = query conn "SELECT * FROM users LIMIT ? OFFSET ?"
            ( fromMaybe 10 limit
            , fromMaybe 0 offset
            )

    findEntity :: Int -> Handler (Maybe User)
    findEntity id_ = liftIO $ withConnection "./sqlite.db" ff
      where
        ff conn = do
          result <- query conn "SELECT * FROM Users WHERE id = ?" [id_]
          return $ listToMaybe result
