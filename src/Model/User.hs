{-# LANGUAGE DeriveGeneric #-}
module Model.User (User(..)) where

import Data.Aeson
import GHC.Generics

newtype User = User
    { userId :: Int
    }
    deriving Generic


instance ToJSON User
instance FromJSON User
