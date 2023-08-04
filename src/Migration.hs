module Migration
  ( run
  ) where

import           Database
import           Database.SQLite3
import           System.Directory

migrationPath :: FilePath
migrationPath = "./sql/migration"

updatePath :: FilePath -> FilePath
updatePath path = migrationPath ++ "/" ++ path

run :: Database -> IO ()
run conn = listDirectory migrationPath
  >>= mapM_ (execQueryDirect conn . updatePath) . reverse
