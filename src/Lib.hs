{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Lib
  ( startApp
  ) where

import           API
import           Data.Function
import           Database.SQLite.Simple
import           Migration
import           Network.Wai.Handler.Warp
import           Servant

settings :: Settings
settings = defaultSettings & setPort 8080 & setHost "127.0.0.1"

startApp :: IO ()
startApp = do
  Migration.run & withConnection "./sqlite.db"
  runSettings settings $ serve (Proxy @API.API) API.server
