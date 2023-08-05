{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.User
  ( User(..)
  ) where

import           Data.Aeson
import           Database.SQLite.Simple
import           Text.Blaze.Html
import qualified Data.Text              as T
import qualified Text.Blaze.Html5       as H

data User = User
  { userId        :: Int
  , userFirstName :: T.Text
  , userLastName  :: T.Text
  } deriving Show

instance ToJSON User where
  toJSON :: User -> Value
  toJSON x = object
    [ "id"         .= userId x
    , "firstName"  .= userFirstName x
    , "lastName"   .= userLastName x
    ]

  toEncoding :: User -> Encoding
  toEncoding x = pairs
    ( "id"          .= userId x
    <> "firstName"  .= userFirstName x
    <> "lastName"   .= userLastName x
    )

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> User
      <$> v .: "id"
      <*> v .: "firstName"
      <*> v .: "lastName"

instance ToRow User where
  toRow :: User -> [SQLData]
  toRow x = toRow
    ( userId x
    , userFirstName x
    , userLastName x
    )

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance H.ToMarkup User where
  toMarkup :: User -> H.Markup
  toMarkup x =
      H.tr $ do
        H.td (toMarkup . T.pack . show $ userId x)
        H.td (toMarkup $ userFirstName x)
        H.td (toMarkup $ userLastName x)

instance H.ToMarkup [User] where
  toMarkup :: [User] -> H.Markup
  toMarkup xs =
      H.table $ do
        H.thead $ do
            H.tr $ do
              H.th (H.text "Id")
              H.th (H.text "First name")
              H.th (H.text "Last name")

        H.tbody (mapM_ toMarkup xs)
          
instance H.ToMarkup (Maybe User) where
  toMarkup :: Maybe User -> H.Markup
  toMarkup = maybe "" toMarkup
