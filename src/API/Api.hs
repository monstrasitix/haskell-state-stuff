{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module API.Api (APIVersion, EntityAPI, entityServer) where

import GHC.TypeLits
import Servant
import Data.Maybe
import qualified Data.List as L


type APIVersion (s :: Symbol) rest = "api" :> s :> rest


type EntityAPI a =
    QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [a]
    :<|> Capture "id" Int :> Get '[JSON] (Maybe a)



entityServer :: Server (EntityAPI a)
entityServer = getEntities :<|> findEntity
    where
        getEntities :: Maybe Int -> Maybe Int -> Handler [a]
        getEntities limit offset = pure . limit' . offset' $ []
            where
                limit' = take $ fromMaybe 0 limit
                offset' = drop $ fromMaybe 0 offset

        findEntity :: Int -> Handler (Maybe a)
        findEntity id_ = pure $ L.find (\_-> id_ == 1) []