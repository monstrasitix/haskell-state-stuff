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
import Data.Foldable (foldrM)

type API
  = QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [User]
  :<|> Capture "id" Int :> Get '[JSON] (Maybe User)


getUser :: Statement -> IO User
getUser stmt = do
  [SQLInteger one, SQLText two, SQLText three] <- typedColumns stmt
    [Just IntegerColumn, Just TextColumn, Just TextColumn]
  return $ User (fromIntegral one) two three

getUsers :: Statement -> IO [User]
getUsers stmt = aggregate [] (step stmt)
  where
    aggregate :: [User] -> StepResult -> IO [User]
    aggregate xs Row = do
      user <- getUser stmt
      return $ user : (aggregare )
    aggregate xs Done = return xs


  -- result <- step stmt
  -- case result of
  --   Row -> do
  --     user <- getUser stmt
  --     return [user]
  --   Done -> return [User 0 "" ""]

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
      
      users <- getUsers stmt
      finalize stmt
      close conn
      return users

    findEntity :: Int -> Handler (Maybe User)
    findEntity _ = return Nothing
