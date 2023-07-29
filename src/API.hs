{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module API
  ( API
  , server
  ) where

import qualified API.Product  as Product
import qualified API.User     as User
import           GHC.TypeLits
import           Servant

type APIVersion (s :: Symbol) rest = "api" :> s :> rest

type API = APIVersion "v1"
  (
    "users" :> User.API
    :<|> "products" :> Product.API
  )

server :: Server API
server = User.server
  :<|> Product.server
