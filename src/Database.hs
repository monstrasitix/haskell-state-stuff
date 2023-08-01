module Database
    ( readQueryDirect
    , execQueryDirect
    , withConnectionDirect
    , withConnection
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

withConnectionDirect :: T.Text -> (D.Database -> IO ()) -> IO ()
withConnectionDirect db ff = do
    conn <- D.open db
    ff conn
    D.close conn

withConnection :: String -> (S.Connection -> IO a) -> IO a
withConnection = S.withConnection

withDatabase :: (S.Connection -> IO a) -> IO a
withDatabase = withConnection "./sqlite.db"
