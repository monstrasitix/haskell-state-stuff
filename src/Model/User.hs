{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs      #-}

module Model.User
  ( User(..)
  ) where

import           Data.Aeson
import qualified Data.Text              as T
import           Database.SQLite.Simple

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