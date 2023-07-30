{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Model.Product
  ( Product(..)
  ) where

import           Data.Aeson
import qualified Data.Text                      as T
import           GHC.Generics

data Product = Product
  { productId   :: Int
  , productName :: T.Text
  } deriving (Show, Generic)

instance FromJSON Product where
  parseJSON = withObject "Product"
    $ \v -> Product
      <$> v .: "id"
      <*> v .: "name"

instance ToJSON Product where
  toEncoding x = pairs
    ( "id"    .= productId x
    <> "name" .= productName x
    )
