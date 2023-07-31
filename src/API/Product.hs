{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API.Product (
  API
  , server
  ) where

import           Control.Monad.IO.Class
import           Model.Product
import           Database.SQLite.Simple
import           Servant
import           Data.Maybe (fromMaybe, listToMaybe)

type API
  = QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [Product]
  :<|> Capture "id" Int :> Get '[JSON] (Maybe Product)

server :: Server API
server = getEntities :<|> findEntity
  where
    getEntities :: Maybe Int -> Maybe Int -> Handler [Product]
    getEntities limit offset = liftIO $ withConnection "./sqlite.db" ff
      where
        ff conn = do
          query conn "SELECT * FROM product LIMIT ? OFFSET ?"
            ( fromMaybe 10 limit
            , fromMaybe 0 offset
            )

    findEntity :: Int -> Handler (Maybe Product)
    findEntity id_ = liftIO $ withConnection "./sqlite.db" ff
      where
        ff conn = do
          rows <- query conn "SELECT * FROM product WHERE id = ?" [id_]
          return $ listToMaybe rows

