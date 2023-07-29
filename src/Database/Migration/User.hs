{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Database.Migration.User
  ( up
  , down
  ) where

import           Database.SQLite.Simple
import           Database.SQLite.Simple.QQ

down :: Connection -> IO ()
down conn = execute_ conn "DROP TABLE IF EXISTS users"

up :: Connection -> IO ()
up conn = do
  execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY
      , firstName VARCHAR(255) NOT NULL
      , lastName VARCHAR(255) NOT NULL
    )
  |]
  execute_ conn [sql|
    INSERT INTO users (id, firstName, lastName) VALUES
      (1, "John", "Doe")
      , (2, "Sally", "Murphy")
      , (3, "Bob", "Dylan")
      , (4, "Sue", "Conor")
      , (5, "Mark", "Jones")
      , (6, "Katie", "Doe")
      , (7, "Michael", "Jordan")
      , (8, "Gwyn", "Murk")
    |]
