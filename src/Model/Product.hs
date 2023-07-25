{-# LANGUAGE DeriveGeneric #-}
module Model.Product (Product(..)) where

import Data.Aeson
import GHC.Generics

newtype Product = Product
    { productId :: Int
    }
    deriving Generic


instance ToJSON Product
instance FromJSON Product
