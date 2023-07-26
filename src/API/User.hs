{-# LANGUAGE OverloadedStrings #-}
module API.User (API, server) where

import API.Root
import Servant
import Model.User

type API = EntityAPI User

server :: Server (EntityAPI User)
server = entityServer
    "SELECT * FROM users LIMIT ? OFFSET ?"
    "SELECT * FROM users WHERE id = ?"