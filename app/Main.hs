{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Attoparsec
import Network.HTTP.Req

getResource :: MonadHttp m => m BsResponse
getResource = req GET (https "app.pokemon-api.xyz" /: "pokemon" /: "random") NoReqBody bsResponse mempty

main :: IO ()
main = runReq defaultHttpConfig $ do
    r <- getResource
    let 
        resp = responseBody r

    liftIO $ print $ parse json resp
    liftIO $ print resp
