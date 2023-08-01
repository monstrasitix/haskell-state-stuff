{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.SQL.User
  ( dbGetUsers
  , dbFindUser
  , dbReplaceUser
  , dbDeleteUser
  , dbAddUser
  ) where

import           Model.User
import           Database.SQLite.Simple
import           Database.SQLite.Simple.QQ
import qualified Data.Maybe             as M


dbGetUsers :: Connection -> Int -> Int -> IO [User]
dbGetUsers conn limit offset =
  query conn "SELECT * FROM user LIMIT ? OFFSET ?" (limit, offset)


dbFindUser :: Connection -> Int -> IO (Maybe User)
dbFindUser conn id_ = M.listToMaybe
  <$> query conn "SELECT * FROM user WHERE id = ?" (Only id_)


dbReplaceUser :: Connection -> Int -> User -> IO (Maybe User)
dbReplaceUser conn id_ x =
  Just x <$ execute conn [sql|
    UPDATE user
    SET firstName = ?,
        lastName = ?
    WHERE id = ?
  |]
    ( userFirstName x
    , userLastName x
    , id_
    )


dbDeleteUser :: Connection -> Int -> IO ()
dbDeleteUser conn id_ =
  execute conn "DELETE FROM user WHERE id = ?" (Only id_)


dbAddUser :: Connection -> User -> IO User
dbAddUser conn x = 
  x <$ execute conn "INSERT INTO user (id, firstName, lastName) VALUES (?, ?, ?)"
    ( userId x
    , userFirstName x
    , userLastName x
    )
