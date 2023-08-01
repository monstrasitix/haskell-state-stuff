{-# LANGUAGE OverloadedStrings #-}

module Database.SQL.Product
  ( dbGetProducts
  , dbFindProduct
  , dbReplaceProduct
  , dbDeleteProduct
  , dbAddProduct
  ) where

import           Model.Product
import           Database.SQLite.Simple
import qualified Data.Maybe             as M


dbGetProducts :: Connection -> Int -> Int -> IO [Product]
dbGetProducts conn limit offset =
  query conn "SELECT * FROM product LIMIT ? OFFSET ?" (limit, offset)


dbFindProduct :: Connection -> Int -> IO (Maybe Product)
dbFindProduct conn id_ = M.listToMaybe
  <$> query conn "SELECT * FROM product WHERE id = ?" (Only id_)


dbReplaceProduct :: Connection -> Int -> Product -> IO (Maybe Product)
dbReplaceProduct conn id_ x =
  Just x <$ execute conn "UPDATE product SET name = ? WHERE id = ?" (productName x, id_)


dbDeleteProduct :: Connection -> Int -> IO ()
dbDeleteProduct conn id_ =
  execute conn "DELETE FROM product WHERE id = ?" (Only id_)


dbAddProduct :: Connection -> Product -> IO Product
dbAddProduct conn x = 
  x <$ execute conn "INSERT INTO product (id, name) VALUES (?, ?)"
    ( productId x
    , productName x
    )
