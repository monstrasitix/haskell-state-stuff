{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Product (Product(..)) where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow


data Product = Product
    { productId :: Int
    , productName :: T.Text
    }
    deriving (Show, Generic)


instance FromJSON Product

instance ToJSON Product where
  toEncoding x = pairs
    ( "id" .= productId x
    <> "name" .= productName x
    )


instance FromRow Product where
  fromRow = Product <$> field <*> field

instance ToRow Product where
  toRow (Product id_ name) = toRow (id_, name)