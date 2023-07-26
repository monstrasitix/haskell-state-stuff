{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Migration.Product (up, down) where

import Database.SQLite.Simple
import Database.SQLite.Simple.QQ


down :: Connection -> IO ()
down conn = execute_ conn "DROP TABLE IF EXISTS products"


up :: Connection -> IO ()
up conn = do
    execute_ conn [sql|
        CREATE TABLE IF NOT EXISTS products (
            id INTEGER PRIMARY KEY
            , name VARCHAR(255) NOT NULL
        )
    |]

    execute_ conn [sql|
        INSERT INTO products (id, name) VALUES
            (1, "Shirt 1")
            , (2, "Shirt 2")
            , (3, "Shirt 3")
            , (4, "Shirt 4")
            , (5, "Shirt 5")
            , (6, "Shirt 6")
            , (7, "Shirt 7")
            ;
    |]