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
import qualified    Data.Text               as T
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
      prev              :: Maybe [String]
    , next              :: Maybe [[String]]
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

instance ToJSON Pokemon

instance FromJSON PokemonName
instance ToJSON PokemonName

instance ToJSON PokemonBase

instance FromJSON PokemonEvolution
instance ToJSON PokemonEvolution

instance FromJSON PokemonProfile
instance ToJSON PokemonProfile

getPikachu :: MonadHttp m => m BsResponse
getPikachu = req GET (https "app.pokemon-api.xyz" /: "pokemon" /: "pikachu") NoReqBody bsResponse mempty

getResourceByID :: MonadHttp m => String -> m BsResponse
getResourceByID pokeID = req GET (https "app.pokemon-api.xyz" /: "pokemon" /: T.pack pokeID) NoReqBody bsResponse mempty

parseResp :: BsResponse -> Pokemon
parseResp = fromJust . decode . BL.fromStrict . responseBody

main :: IO ()
main = runReq defaultHttpConfig $ do
  r <- getPikachu
  let 
      pokeData      = parseResp r

      id            = view (field @"id") pokeData

      pokeNameEng   = view (field @"name" . field @"english") pokeData

      pokeEvo       = evolution pokeData

      pokePrevEvo   = fromJust $ view (field @"prev") pokeEvo
      idPrev        = head pokePrevEvo
      pokeNextEvo   = fromJust $ view (field @"evolution" . field @"next") pokeData
      idNext        = head $ head pokeNextEvo
  
  rPrev <- getResourceByID idPrev
  rNext <- getResourceByID idNext
  let
      pokeNamePrev  = view (field @"name" . field @"english") $ parseResp rPrev
      pokeNameNext  = view (field @"name" . field @"english") $ parseResp rNext

  liftIO $ print id
  liftIO $ print pokeNameEng
  liftIO $ print pokePrevEvo
  liftIO $ print $ "Previous: " ++ pokeNamePrev
  liftIO $ print pokeNextEvo
  liftIO $ print $ "Next: " ++ pokeNameNext