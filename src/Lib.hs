{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lib ( startApp ) where

import Servant
import Servant.HTML.Blaze
import GHC.Generics
import Network.Wai.Handler.Warp
import Data.Aeson
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time.Calendar
import Text.Blaze
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H

data User = User
  { userId :: Int
  , userFirstName :: T.Text
  , userLastName :: T.Text
  , userDateOfBirth :: Day
  }
  deriving (Show, Generic)


instance ToJSON User where
  toJSON :: User -> Value
  toJSON user = object
    [ "id"          .= userId user
    , "firstName"   .= userFirstName user
    , "lastName"    .= userLastName user
    , "dateOfBirth" .= userDateOfBirth user
    ]

  toEncoding :: User -> Encoding
  toEncoding user = pairs
    ( "id"           .= userId user
    <> "firstName"   .= userFirstName user
    <> "lastName"    .= userLastName user
    <> "dateOfBirth" .= userDateOfBirth user
    )

instance FromJSON User where
  parseJSON = withObject "User" $ \v->
    User
      <$> v .: "id"
      <*> v .: "firstName"
      <*> v .: "lastName"
      <*> v .: "dateOfBirth"

instance ToMarkup User where
  toMarkup user =
    H.tr $ do
      H.td (toHtml $ userFirstName user)
      H.td (toHtml $ userLastName user)

instance ToMarkup (Maybe User) where
  toMarkup (Just user) = toMarkup user
  toMarkup Nothing = ""

instance ToMarkup [User] where
  toMarkup xs = H.table $ do
    H.tr $ do
      H.th "First name"
      H.th "Last name"
    foldMap toMarkup xs


type API = "users" :>
  (
    Get '[JSON, HTML] [User]
    :<|> Capture "userId" Int :> Get '[JSON, HTML] (Maybe User)
  )


users :: [User]
users =
    [ User 1 (T.pack "John") (T.pack "Doe") (fromGregorian 1995 9 11)
    , User 2 (T.pack "Jane") (T.pack "Doe") (fromGregorian 1995 9 11)
    ]


server :: Server API
server = getUsers :<|> findUser
  where
    getUsers :: Handler [User]
    getUsers = return users

    findUser :: Int -> Handler (Maybe User)
    findUser id_ = return $ L.find ff users
      where
          ff = (== id_) . userId


startApp :: IO ()
startApp = run 8080 $ serve (Proxy @API) server