{-# LANGUAGE OverloadedStrings #-}

module Database.SQL.Product
  ( dbGetProducts
  , dbFindProduct
  ) where

import           Model.Product
import           Database.SQLite.Simple
import qualified Data.Maybe             as M

dbGetProducts :: Connection -> Int -> Int -> IO [Product]
dbGetProducts conn limit offset = query conn
  "SELECT * FROM product LIMIT ? OFFSET ?"
  (limit, offset)

dbFindProduct :: Connection -> Int -> IO (Maybe Product)
dbFindProduct conn id_ = M.listToMaybe
  <$> query conn "SELECT * FROM product WHERE id = ?" [id_]
