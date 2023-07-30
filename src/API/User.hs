{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API.User (
  API
  , server
  ) where

import           Control.Monad.IO.Class
import           Database.SQLite3
import           Model.User
import           Servant
import           Data.Maybe (fromMaybe)

type API
  = QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [User]
  :<|> Capture "id" Int :> Get '[JSON] (Maybe User)

server :: Server API
server = getEntities :<|> findEntity
  where
    getEntities :: Maybe Int -> Maybe Int -> Handler [User]
    getEntities limit offset = liftIO $ do
      conn <- open "./sqlite.db"
      stmt <- prepare conn "SELECT * FROM user LIMIT ? OFFSET ?"
      bind stmt
        [ SQLInteger . fromIntegral $ fromMaybe 10 limit
        , SQLInteger . fromIntegral $ fromMaybe 0 offset
        ]
      [SQLInteger one, SQLText two, SQLText three] <- typedColumns stmt
        [Just IntegerColumn, Just TextColumn, Just TextColumn]
      finalize stmt
      close conn
      return
        [ User (fromIntegral one) two three]

    findEntity :: Int -> Handler (Maybe User)
    findEntity _ = return Nothing
