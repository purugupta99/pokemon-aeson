{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
-- import Data.Attoparsec
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import Data.Generics.Product
import Data.Generics.Sum
import Data.Maybe
import GHC.Generics (Generic)
import Network.HTTP.Req

data Pokemon = Pokemon {
    id :: Maybe Int,
    -- name :: PokemonName,
    -- pokemonType :: [String],
    -- base :: PokemonBase,
    -- species :: String,
    -- description :: String,
    evolution :: PokemonEvolution,
    -- profile :: PokemonProfile,
    -- image :: Value,
    -- sprite :: String,
    -- thumbnail :: String,
    hires :: String
} deriving (Show, Generic)

data PokemonName = PokemonName {
    english :: String,
    japanese :: String,
    chinese :: String,
    french :: String
} deriving (Show, Generic)

data PokemonBase = PokemonBase {
    hp :: Int,
    attack :: Int,
    defense :: Int,
    specialAttack :: Int,
    specialDefence :: Int,
    speed :: Int
} deriving (Show, Generic)

data PokemonEvolution = PokemonEvolution {
    prev :: Maybe [String],
    next :: Maybe [[String]]
} deriving (Show, Generic)

data PokemonProfile = PokemonProfile {
    height :: String,
    weight :: String,
    egg :: [String],
    ability :: [[String]],
    gender :: String
} deriving (Show, Generic)

instance FromJSON Pokemon
instance ToJSON Pokemon

instance FromJSON PokemonName
instance ToJSON PokemonName

instance FromJSON PokemonBase
instance ToJSON PokemonBase

instance FromJSON PokemonEvolution
instance ToJSON PokemonEvolution

instance FromJSON PokemonProfile
instance ToJSON PokemonProfile

getResource :: MonadHttp m => m BsResponse
getResource = req GET (https "app.pokemon-api.xyz" /: "pokemon" /: "pikachu") NoReqBody bsResponse mempty

main :: IO ()
main = runReq defaultHttpConfig $ do
    r <- getResource
    let 
        resp = responseBody r
        -- jsonResp = parse json resp
        jsonResp :: Maybe Pokemon
        jsonResp = decode $ BL.fromStrict resp
        
        pokeData = fromJust jsonResp

        id = fromJust $ view (field @"id") pokeData
        pokeEvo = evolution pokeData

        pokePrevEvo = fromJust $ view (field @"prev") pokeEvo
        pokeNextEvo = fromJust $ view (field @"evolution" . field @"next") pokeData

        -- id = view (field @"id" ._Just) $ fromJust jsonResp

    liftIO $ print jsonResp
    liftIO $ print id
    liftIO $ print pokePrevEvo
    liftIO $ print pokeNextEvo
