{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.Product
  ( Product(..)
  ) where


import           Data.Aeson
import           Database.SQLite.Simple
import           Text.Blaze.Html
import qualified Data.Text              as T
import qualified Text.Blaze.Html5       as H

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

instance H.ToMarkup Product where
  toMarkup :: Product -> H.Markup
  toMarkup x =
      H.tr $ do
        H.td (toMarkup . T.pack . show $ productId x)
        H.td (toMarkup $ productName x)

instance H.ToMarkup [Product] where
  toMarkup :: [Product] -> H.Markup
  toMarkup xs =
      H.table $ do
        H.thead $ do
            H.tr $ do
              H.th (H.text "Id")
              H.th (H.text "Name")

        H.tbody (mapM_ toMarkup xs)

instance H.ToMarkup (Maybe Product) where
  toMarkup :: Maybe Product -> H.Markup
  toMarkup = maybe "" toMarkup