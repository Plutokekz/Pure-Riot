{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.AccountsRSO.AccountV1 where

import           Data.Aeson          (FromJSON)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           PureRiot            (TypedRequest (TypedRequest))
import           Regions             (Region (..), regionToUrl)

data AccountDto = AccountDto
  { puuid    :: String,
    {- | This field may be excluded from the response if the account doesn't have a gameName.-}
    gameName :: String,
    {- | This field may be excluded from the response if the account doesn't have a tagLine.-}
    tagLine  :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON AccountDto

data ActiveShardDTO = ActiveShardDTO
  { puuid       :: String,
    game        :: String,
    activeShard :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON ActiveShardDTO

data Game = Val | Lor
  deriving (Eq)

instance Show Game where
  show Val = "val"
  show Lor = "lor"

type GameName = String

type Puuid = String

type TagLine = String

accountByPuuid :: Region -> Puuid -> IO (TypedRequest AccountDto)
accountByPuuid region puuid = do
  request <- parseRequest $ regionToUrl region ++ "/riot/account/v1/accounts/by-puuid/" ++ puuid
  return $ TypedRequest request

accountByRiotId :: Region -> GameName -> TagLine -> IO (TypedRequest AccountDto)
accountByRiotId region gameName tagLine = do
  request <- parseRequest $ regionToUrl region ++ "/riot/account/v1/accounts/by-riot-id/" ++ gameName ++ "/" ++ tagLine
  return $ TypedRequest request

accountMe :: Region -> IO (TypedRequest AccountDto)
accountMe region = do
  request <- parseRequest $ regionToUrl region ++ "/riot/account/v1/accounts/me"
  return $ TypedRequest request

activeShardByGameAndPuuid :: Region -> Game -> Puuid -> IO (TypedRequest ActiveShardDTO)
activeShardByGameAndPuuid region game puuid = do
  request <- parseRequest $ regionToUrl region ++ "/riot/account/v1/active-shards/by-game/" ++ show game ++ "/by-puuid/" ++ puuid
  return $ TypedRequest request
