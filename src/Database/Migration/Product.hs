{-# LANGUAGE OverloadedStrings #-}

module Database.Migration.Product
  ( up
  , down
  ) where

import           Database
import           Database.SQLite3

down :: Database -> IO ()
down conn = execQuery conn "./sql/migration/0001-migration-down.sql"

up :: Database -> IO ()
up conn = execQuery conn "./sql/migration/0001-migration-up.sql"
