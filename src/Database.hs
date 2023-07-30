module Database
    ( readQuery
    , execQuery
    , withConnection
    ) where

import qualified Data.Text          as T
import           Data.Functor
import           Database.SQLite3

readQuery :: FilePath -> IO T.Text
readQuery path = readFile path <&> T.pack

execQuery :: Database -> FilePath -> IO ()
execQuery conn path = readQuery path >>= exec conn

withConnection :: T.Text -> (Database -> IO ()) -> IO ()
withConnection db ff = do
    conn <- open db
    ff conn
    close conn