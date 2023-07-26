{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib (startApp) where

import Data.Function
import Network.Wai.Handler.Warp
import Servant

import API.Root
import qualified API.User as User
import qualified API.Product as Product
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ


type API = APIVersion "v1"
    (
        "users" :> User.API
        :<|> "products" :> Product.API
    )


doDatabaseStuff :: IO ()
doDatabaseStuff = do
    conn <- open "./sqlite.db"
    execute_ conn [sql|
        CREATE TABLE IF NOT EXISTS users (
            id INTEGER PRIMARY key
            , firstName VARCHAR(255) NOT NULL
            , lastName VARCHAR(255) NOT NULL
        )
    |]

    execute_ conn [sql|
        CREATE TABLE IF NOT EXISTS products (
            id INTEGER PRIMARY key
            , name VARCHAR(255) NOT NULL
        )
    |]
    close conn

server :: Server API
server = User.server
    :<|> Product.server


settings :: Settings
settings = defaultSettings
    & setPort 8080
    & setHost "127.0.0.1"


initializeDB :: FilePath -> IO ()
initializeDB path = withConnection path $ \conn-> do
    execute_ conn "DROP TABLE IF EXISTS users"
    execute_ conn "DROP TABLE IF EXISTS products"

    execute_ conn [sql|
        CREATE TABLE IF NOT EXISTS users (
            id INTEGER PRIMARY KEY
            , firstName VARCHAR(255) NOT NULL
            , lastName VARCHAR(255) NOT NULL
        )
    |]

    execute_ conn [sql|
        INSERT INTO users (id, firstName, lastName) VALUES
            (1, "John", "Doe"),
            (2, "John", "Doe"),
            (3, "John", "Doe"),
            (4, "John", "Doe"),
            (5, "John", "Doe"),
            (6, "John", "Doe"),
            (7, "John", "Doe");
    |]

    execute_ conn [sql|
        CREATE TABLE IF NOT EXISTS products (
            id INTEGER PRIMARY KEY
            , name VARCHAR(255) NOT NULL
        )
    |]

    execute_ conn [sql|
        INSERT INTO products (id, name) VALUES
            (1, "Shirt 1"),
            (2, "Shirt 2"),
            (3, "Shirt 3"),
            (4, "Shirt 4"),
            (5, "Shirt 5"),
            (6, "Shirt 6"),
            (7, "Shirt 7");
    |]

startApp :: IO ()
startApp = do
    initializeDB "./sqlite.db"
    doDatabaseStuff
    runSettings settings
        $ serve (Proxy @API) server
