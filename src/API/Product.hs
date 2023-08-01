{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API.Product
  ( API
  , server
  ) where

import           Control.Monad.IO.Class
import           Model.Product
import           Servant
import           Database
import           Database.SQL.Product
import qualified Data.Maybe             as M

type API =
  (
    QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [Product]
    :<|> ReqBody '[JSON] Product :> Post '[JSON] Product
    :<|> Capture "id" Int :> Get '[JSON] (Maybe Product)
    :<|> Capture "id" Int :> DeleteNoContent
  )

server :: Server API
server = getEntities :<|> createEntity :<|> findEntity :<|> deleteEntity
  where
    getEntities :: Maybe Int -> Maybe Int -> Handler [Product]
    getEntities limit offset = liftIO . withDatabase
      $ \conn -> dbGetProducts conn
        (M.fromMaybe 10 limit)
        (M.fromMaybe 0 offset)
    
    createEntity :: Product -> Handler Product
    createEntity x = liftIO . withDatabase
      $ \conn -> dbAddProduct conn x

    findEntity :: Int -> Handler (Maybe Product)
    findEntity id_ = liftIO . withDatabase
      $ \conn -> dbFindProduct conn id_

    deleteEntity :: Int -> Handler NoContent
    deleteEntity id_ = liftIO . withDatabase
      $ \conn -> NoContent <$ dbDeleteProduct conn id_
