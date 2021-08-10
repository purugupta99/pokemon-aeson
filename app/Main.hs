{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Main where

import              Control.Lens
import              Control.Monad.IO.Class
import              Data.Aeson
import              Data.Aeson.TH
import qualified    Data.ByteString.Lazy    as BL
import              Data.Generics.Product
import              Data.Maybe
import              GHC.Generics            (Generic)
import              Network.HTTP.Req

data Pokemon = Pokemon
  {
      id                :: Int
    , name              :: PokemonName
    , pokemonType       :: [String]
    , base              :: PokemonBase
    , species           :: String
    , description       :: String
    , evolution         :: PokemonEvolution
    , profile           :: PokemonProfile
    , image             :: Value
    , sprite            :: String
    , thumbnail         :: String
    , hires             :: String
  }
  deriving (Show, Generic)

data PokemonName = PokemonName
  { 
      english           :: String
    , japanese          :: String
    , chinese           :: String
    , french            :: String
  }
  deriving (Show, Generic)

data PokemonBase = PokemonBase
  { 
      hp                :: Int
    , attack            :: Int
    , defense           :: Int
    , specialAttack     :: Int
    , specialDefence    :: Int
    , speed             :: Int
  }
  deriving (Show, Generic)

data PokemonEvolution = PokemonEvolution
  { 
      prev              :: [String]
    , next              :: [[String]]
  }
  deriving (Show, Generic)

data PokemonProfile = PokemonProfile
  { 
      height            :: String
    , weight            :: String
    , egg               :: [String]
    , ability           :: [[String]]
    , gender            :: String
  }
  deriving (Show, Generic)

$( deriveFromJSON
     defaultOptions
       {
           fieldLabelModifier =
            let f "pokemonType"      = "type"
                f other              = other
                in f
       }
     ''Pokemon
 )

$( deriveFromJSON
     defaultOptions
       {
           fieldLabelModifier =
            let f "hp"               = "HP"
                f "attack"           = "Attack"
                f "defense"          = "Defense"
                f "specialAttack"    = "Sp. Attack"
                f "specialDefence"   = "Sp. Defense"
                f "speed"            = "Speed"
                f other              = other
                in f
       }
     ''PokemonBase
 )


-- instance FromJSON Pokemon
instance ToJSON Pokemon

instance FromJSON PokemonName
instance ToJSON PokemonName

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
  let resp = responseBody r
      -- jsonResp = parse json resp
      jsonResp :: Maybe Pokemon
      jsonResp      = decode $ BL.fromStrict resp
      pokeData      = fromJust jsonResp

      id            = view (field @"id") pokeData

      pokeNameEng   = view (field @"name" . field @"english") pokeData

      pokeEvo       = evolution pokeData

      pokePrevEvo   = view (field @"prev") pokeEvo
      pokeNextEvo   = view (field @"evolution" . field @"next") pokeData

  -- id = view (field @"id" ._Just) $ fromJust jsonResp

  liftIO $ print jsonResp
  liftIO $ print id
  liftIO $ print pokeNameEng
  liftIO $ print pokePrevEvo
  liftIO $ print pokeNextEvo