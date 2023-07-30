module Migration
  ( run
  ) where

import Database.SQLite3

import qualified Database.Migration.Product as Product
-- import qualified Database.Migration.User    as User

up :: Database -> IO ()
up conn = do
  Product.up conn
  -- User.up conn

down :: Database -> IO ()
down conn = do
  Product.down conn
  -- User.down conn

run :: Database -> IO ()
run conn =
  down conn
  >> up conn
