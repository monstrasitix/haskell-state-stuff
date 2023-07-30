{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API.Product (
  API
  , server
  ) where

import           Control.Monad.IO.Class
import           Model.Product
import           Database.SQLite3
import           Servant
import           Data.Maybe (fromMaybe)

type API
  = QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [Product]
  :<|> Capture "id" Int :> Get '[JSON] (Maybe Product)

server :: Server API
server = getEntities :<|> findEntity
  where
    getEntities :: Maybe Int -> Maybe Int -> Handler [Product]
    getEntities limit offset = liftIO $ do
      conn <- open "./sqlite.db"
      stmt <- prepare conn "SELECT * FROM user LIMIT ? OFFSET ?"
      bind stmt
        [ SQLInteger . fromIntegral $ fromMaybe 10 limit
        , SQLInteger . fromIntegral $ fromMaybe 0 offset
        ]
      [SQLInteger one, SQLText two] <- typedColumns stmt
        [Just IntegerColumn, Just TextColumn]
      finalize stmt
      close conn
      return
        [ Product (fromIntegral one) two]

    -- getEntities limit offset = liftIO $ withConnection "./sqlite.db" ff
    --   where
    --     ff conn = query conn "SELECT * FROM products LIMIT ? OFFSET ?"
    --         ( fromMaybe 10 limit
    --         , fromMaybe 0 offset
    --         )
    
    findEntity :: Int -> Handler (Maybe Product)
    findEntity _ = return Nothing

    -- findEntity :: Int -> Handler (Maybe Product)
    -- findEntity id_ = liftIO $ withConnection "./sqlite.db" ff
    --   where
    --     ff conn = do
    --       result <- query conn "SELECT * FROM products WHERE id = ?" [id_]
    --       return $ listToMaybe result
