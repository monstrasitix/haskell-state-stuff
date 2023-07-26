{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module API.Root (APIVersion, EntityAPI, entityServer) where

import GHC.TypeLits
import Servant
import Data.Maybe
import Database.SQLite.Simple
import Control.Monad.IO.Class

type APIVersion (s :: Symbol) rest = "api" :> s :> rest


type EntityAPI a =
    QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [a]
    :<|> Capture "id" Int :> Get '[JSON] (Maybe a)


entityServer :: FromRow a => Query -> Query -> Server (EntityAPI a)
entityServer q1 q2 = getEntities :<|> findEntity
    where
        getEntities :: FromRow a => Maybe Int -> Maybe Int -> Handler [a]
        getEntities limit offset = liftIO $
            withConnection "./sqlite.db" ff
            where
                ff :: FromRow a => Connection -> IO [a]
                ff conn = query conn q1
                    ( fromMaybe 10 limit
                    , fromMaybe 0 offset
                    )
            
        findEntity :: FromRow a => Int -> Handler (Maybe a)
        findEntity id_ = liftIO $ withConnection "./sqlite.db" ff
            where
                ff :: FromRow a => Connection -> IO (Maybe a)
                ff conn = do
                    result <- query conn q2 [id_]
                    return $ listToMaybe result