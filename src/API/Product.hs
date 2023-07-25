module API.Product (API, server) where

import API.Api
import Servant
import Model.Product

type API = EntityAPI Product

server :: Server (EntityAPI Product)
server = entityServer