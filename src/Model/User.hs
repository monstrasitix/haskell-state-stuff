{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs      #-}

module Model.User
  ( User(..)
  ) where

import           Data.Aeson
import qualified Data.Text                      as T
import           GHC.Generics

data User = User
  { userId        :: Int
  , userFirstName :: T.Text
  , userLastName  :: T.Text
  } deriving (Show, Generic)

instance ToJSON User where
  toEncoding user = pairs
    ( "id"          .= userId user
    <> "firstName"  .= userFirstName user
    <> "lastName"   .= userLastName user
    )

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> User
      <$> v .: "id"
      <*> v .: "firstName"
      <*> v .: "lastName"
