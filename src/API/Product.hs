{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API.Product
  ( API
  , server
  ) where

import           Control.Monad.IO.Class
import           Database
import           Database.SQL.Product
import           Html
import           Model.Product
import           Servant
import qualified Data.Maybe             as M

type API
  = QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON, PrettyHTML] [Product]
  :<|> ReqBody '[JSON] Product :> Post '[JSON, PrettyHTML] Product
  :<|> Capture "id" Int :> 
    (
      Get '[JSON, PrettyHTML] (Maybe Product)
      :<|> ReqBody '[JSON] Product :> Put '[JSON, PrettyHTML] (Maybe Product)
      :<|> DeleteNoContent
    )

server :: Server API
server = getEntities
  :<|> createEntity
  :<|> capture
  where
    capture id_ = findEntity id_
      :<|> replaceEntity id_
      :<|> deleteEntity id_

getEntities :: Maybe Int -> Maybe Int -> Handler [Product]
getEntities limit offset = liftIO . withDatabase
  $ \conn -> dbGetProducts conn
    (M.fromMaybe 10 limit)
    (M.fromMaybe 0 offset)

createEntity :: Product -> Handler Product
createEntity entity = liftIO . withDatabase
  $ \conn -> dbAddProduct conn entity

findEntity :: Int -> Handler (Maybe Product)
findEntity id_ = liftIO . withDatabase
  $ \conn -> dbFindProduct conn id_

replaceEntity :: Int -> Product -> Handler (Maybe Product)
replaceEntity id_ entity = liftIO . withDatabase
  $ \conn -> dbReplaceProduct conn id_ entity

deleteEntity :: Int -> Handler NoContent
deleteEntity id_ = liftIO . withDatabase
  $ \conn -> NoContent <$ dbDeleteProduct conn id_
