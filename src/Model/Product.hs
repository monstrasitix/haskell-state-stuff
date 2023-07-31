{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs      #-}

module Model.Product
  ( Product(..)
  ) where

import           Data.Aeson
import qualified Data.Text    as T
import           Database.SQLite.Simple

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
