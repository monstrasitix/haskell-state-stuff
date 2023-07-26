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
    parseJSON = withObject "User" $ \x -> User
        <$> x .: "id"
        <*> x .: "firstName"
        <*> x .: "lastName"

instance FromRow User where
  fromRow = User
    <$> field
    <*> field
    <*> field

instance ToRow User where
  toRow (User id_ firstName lastName) = toRow
    ( id_
    , firstName
    , lastName
    )