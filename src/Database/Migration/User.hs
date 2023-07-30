{-# LANGUAGE OverloadedStrings #-}

module Database.Migration.User
  ( up
  , down
  ) where

import           Database
import           Database.SQLite3

down :: Database -> IO ()
down conn = execQuery conn "./sql/migration/0002-migration-down.sql"

up :: Database -> IO ()
up conn = execQuery conn "./sql/migration/0002-migration-up.sql"
