{-# LANGUAGE OverloadedStrings #-}

module Database
    ( readQueryDirect
    , execQueryDirect
    , withDatabaseDirect
    , withDatabase
    ) where

import           Data.Functor
import qualified Data.Text              as T
import qualified Database.SQLite3       as D
import qualified Database.SQLite.Simple as S

readQueryDirect :: FilePath -> IO T.Text
readQueryDirect path = readFile path <&> T.pack

execQueryDirect :: D.Database -> FilePath -> IO ()
execQueryDirect conn path = readQueryDirect path >>= D.exec conn

withDatabaseDirect :: (D.Database -> IO ()) -> IO ()
withDatabaseDirect ff = do
    conn <- D.open "./sqlite.db"
    ff conn
    D.close conn

withDatabase :: (S.Connection -> IO a) -> IO a
withDatabase = S.withConnection "./sqlite.db"
