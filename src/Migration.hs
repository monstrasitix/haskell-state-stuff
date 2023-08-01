module Migration
  ( run
  ) where

import Database
import Database.SQLite3


run :: Database -> IO ()
run conn = do
  execQueryDirect conn "./sql/migration/0001-migration-down.sql"
  execQueryDirect conn "./sql/migration/0001-migration-up.sql"

  execQueryDirect conn "./sql/migration/0002-migration-down.sql"
  execQueryDirect conn "./sql/migration/0002-migration-up.sql"
