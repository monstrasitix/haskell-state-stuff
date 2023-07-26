{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.User (User(..)) where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow


data User = User
    { userId :: Int
    , userFirstName :: T.Text
    , userLastName :: T.Text
    }
    deriving Generic


instance ToJSON User where
    toEncoding user = pairs
        ( "id" .= userId user
        <> "firstName" .= userFirstName user
        <> "lastName" .= userLastName user
        )

    
instance FromJSON User where
    parseJSON = withObject "User" $ \v -> User
        <$> v .: "id"
        <*> v .: "firstName"
        <*> v .: "lastName"


instance FromRow User where
  fromRow = User
    <$> field
    <*> field
    <*> field


instance ToRow User where
  toRow x = toRow
    ( userId x
    , userFirstName x
    , userLastName x
    )
