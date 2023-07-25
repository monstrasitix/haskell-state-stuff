{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib (startApp) where

import Data.Function
import Network.Wai.Handler.Warp
import Servant

import API.Api
import qualified API.User as User
import qualified API.Product as Product


type API = APIVersion "v1"
    (
        "users" :> User.API
        :<|> "products" :> Product.API
    )

server :: Server API
server = User.server
    :<|> Product.server


settings :: Settings
settings = defaultSettings
    & setPort 8080
    & setHost "127.0.0.1"


startApp :: IO ()
startApp = runSettings settings
    $ serve (Proxy @API) server
