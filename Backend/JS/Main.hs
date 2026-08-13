{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Proxy
import Data.Text as T (Text)
import Data.Text.IO as T (writeFile)

import Servant
import Servant.JS

import API.Endpoint.Place
import API.Endpoint.Trip

type API = "localhost:8081" :> (PlaceAPI :<|> TripAPI)
api :: Proxy API
api = Proxy

apiAxios :: Text
apiAxios = jsForAPI api $ axios defAxiosOptions

apiJS :: Text
apiJS = jsForAPI api vanillaJS

main :: IO ()
main = do
  T.writeFile "axiosApi.js" apiAxios
