module Migration (run) where

import Database.SQLite.Simple

import qualified Database.Migration.Product as Product
import qualified Database.Migration.User as User


up :: Connection -> IO ()
up conn = do
    Product.up conn
    User.up conn


down :: Connection -> IO ()
down conn = do
    Product.down conn
    User.down conn


run :: Connection -> IO ()
run conn = down conn >> up conn
