{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Lib
  ( startApp
  ) where

import qualified API
import           Data.Function
import           Database
import           Migration
import           Network.Wai.Handler.Warp
import           Servant

settings :: Settings
settings = defaultSettings
  & setPort 8080
  & setHost "127.0.0.1"

app :: Application
app = serve (Proxy @API.API) API.server

startApp :: IO ()
startApp = do
  withConnection "./sqlite.db" Migration.run
  runSettings settings app
