module API.User (API, server) where

import API.Api
import Servant
import Model.User

type API = EntityAPI User

server :: Server (EntityAPI User)
server = entityServer