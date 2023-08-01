module Migration
  ( run
  ) where

import           Database
import           Database.SQLite3
import           System.Directory

migrationPath :: FilePath
migrationPath = "./sql/migration"

-- TODO: Path ordering will be incorrect
run :: Database -> IO ()
run conn = listDirectory migrationPath
  >>= mapM_ (execQueryDirect conn)
