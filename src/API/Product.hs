{-# LANGUAGE OverloadedStrings #-}
module API.Product (API, server) where

import API.Root
import Servant
import Model.Product

type API = EntityAPI Product

server :: Server (EntityAPI Product)
server = entityServer
    "SELECT * FROM products LIMIT ? OFFSET ?"
    "SELECT * FROM products WHERE id = ?"