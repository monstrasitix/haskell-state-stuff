{-# LANGUAGE OverloadedStrings #-}

module Database.SQL.Product
  ( dbGetProducts
  , dbFindProduct
  , dbDeleteProduct
  , dbAddProduct
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

dbDeleteProduct :: Connection -> Int -> IO ()
dbDeleteProduct conn id_ =
  execute conn "DELETE FROM product WHERE id = ?" [id_]

dbAddProduct :: Connection -> Product -> IO Product
dbAddProduct conn x = do
  execute conn "INSERT INTO product (id, name) VALUES (?, ?)"
    ( productId x
    , productName x
    )
  return x
