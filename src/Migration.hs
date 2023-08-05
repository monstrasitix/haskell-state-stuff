module Migration
  ( run
  ) where

import           Database
import           Database.SQLite3
import           System.Directory
import           Text.Printf

migrationPath :: FilePath
migrationPath = "./sql/migration"

updatePath :: FilePath -> FilePath
updatePath = printf "%s/%s" migrationPath

run :: Database -> IO ()
run conn = listDirectory migrationPath
  >>= mapM_ (execQueryDirect conn . updatePath) . reverse
