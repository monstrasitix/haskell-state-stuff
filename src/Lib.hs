{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startApp
  ) where

import qualified API
import           Data.Function
import           Database.SQLite.Simple
import           Migration
import           Network.Wai.Handler.Warp
import           Servant

settings :: Settings
settings = defaultSettings
  & setPort 8080
  & setHost "127.0.0.1"

api :: Proxy API.API
api = Proxy

app :: Application
app = serve api API.server

startApp :: IO ()
startApp = do
  withConnection "./sqlite.db" Migration.run
  runSettings settings app
