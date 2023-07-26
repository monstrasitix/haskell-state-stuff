{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module API (API, server) where

import GHC.TypeLits
import Servant
import qualified API.User as User
import qualified API.Product as Product


type APIVersion (s :: Symbol) rest = "api" :> s :> rest


type API = APIVersion "v1"
    (
        "users" :> User.API
        :<|> "products" :> Product.API
    )


server :: Server API
server = User.server
    :<|> Product.server