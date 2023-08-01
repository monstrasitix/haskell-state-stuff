{-# LANGUAGE OverloadedStrings #-}

module Database.SQL.User
  ( dbGetUsers
  , dbFindUser
  ) where

import           Model.User
import           Database.SQLite.Simple
import qualified Data.Maybe             as M

dbGetUsers :: Connection -> Int -> Int -> IO [User]
dbGetUsers conn limit offset = query conn
  "SELECT * FROM user LIMIT ? OFFSET ?"
  (limit, offset)

dbFindUser :: Connection -> Int -> IO (Maybe User)
dbFindUser conn id_ = M.listToMaybe
  <$> query conn "SELECT * FROM user WHERE id = ?" [id_]
