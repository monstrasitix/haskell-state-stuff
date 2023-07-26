{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
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


instance FromJSON Product where
  parseJSON = withObject "Product" $ \v -> Product
    <$> v .: "id"
    <*> v .: "name"


instance ToJSON Product where
  toEncoding :: Product -> Encoding
  toEncoding x = pairs
    ( "id" .= productId x
    <> "name" .= productName x
    )


instance FromRow Product where
  fromRow :: RowParser Product
  fromRow = Product
    <$> field
    <*> field


instance ToRow Product where
  toRow x = toRow
    ( productId x
    , productName x
    )
