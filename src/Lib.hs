{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib (startApp) where

import Data.Function
import Network.Wai.Handler.Warp
import Servant
import API
import Database.SQLite.Simple
import Migration

settings :: Settings
settings = defaultSettings
    & setPort 8080
    & setHost "127.0.0.1"

startApp :: IO ()
startApp = do
    Migration.run & withConnection "./sqlite.db"
    runSettings settings
        $ serve (Proxy @API.API) API.server
