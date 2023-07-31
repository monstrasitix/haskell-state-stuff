{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs      #-}

module Model.Product
  ( Product(..)
  , dbGetProducts
  , dbFindProduct
  ) where

import           Data.Aeson
import           Database.SQLite.Simple
import qualified Data.Text              as T
import qualified Data.Maybe             as M

data Product = Product
  { productId   :: Int
  , productName :: T.Text
  } deriving Show

instance FromJSON Product where
  parseJSON = withObject "Product"
    $ \v -> Product
      <$> v .: "id"
      <*> v .: "name"

instance ToJSON Product where
  toJSON :: Product -> Value
  toJSON x = object
    [ "id" .= productId x
    , "name" .= productName x
    ]

  toEncoding :: Product -> Encoding
  toEncoding x = pairs
    ( "id"    .= productId x
    <> "name" .= productName x
    )

instance ToRow Product where
  toRow :: Product -> [SQLData]
  toRow x = toRow
    ( productId x
    , productName x
    )

instance FromRow Product where
  fromRow = Product <$> field <*> field

dbGetProducts :: Connection -> Int -> Int -> IO [Product]
dbGetProducts conn limit offset = query conn
  "SELECT * FROM product LIMIT ? OFFSET ?"
  (limit, offset)

dbFindProduct :: Connection -> Int -> IO (Maybe Product)
dbFindProduct conn id_ = M.listToMaybe
  <$> query conn "SELECT * FROM product WHERE id = ?" [id_]
