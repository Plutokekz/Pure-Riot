{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.LeagueofLegends.LeagueV4 where

import           Data.Aeson          (FromJSON)
import           Data.Set            (Set)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data LeagueEntryDTO = LeagueEntryDTO
  { leagueId     :: String,
    {-Player's encrypted summonerId.-}
    summonerId   :: String,
    {-Player's encrypted puuid.-}
    puuid        :: String,
    queueType    :: String,
    tier         :: String,
    {-The player's division within a tier.-}
    rank         :: String,
    leaguePoints :: Int,
    {-Winning team on Summoners Rift.-}
    wins         :: Int,
    {-Losing team on Summoners Rift.-}
    losses       :: Int,
    hotStreak    :: Bool,
    veteran      :: Bool,
    freshBlood   :: Bool,
    inactive     :: Bool,
    miniSeries   :: MiniSeriesDTO
  }
  deriving (Show, Eq, Generic)

instance FromJSON LeagueEntryDTO

data LeagueListDTO = LeagueListDTO
  { leagueId :: String,
    entries  :: [LeagueItemDTO],
    tier     :: String,
    name     :: String,
    queue    :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON LeagueListDTO

data LeagueItemDTO = LeagueItemDTO
  { freshBlood   :: Bool,
    {-Winning team on Summoners Rift.-}
    wins         :: Int,
    miniSeries   :: MiniSeriesDTO,
    inactive     :: Bool,
    veteran      :: Bool,
    hotStreak    :: Bool,
    rank         :: String,
    leaguePoints :: Int,
    {-Losing team on Summoners Rift.-}
    losses       :: Int,
    {-Player's encrypted summonerId.-}
    summonerId   :: String,
    {-Player's encrypted puuid.-}
    puuid        :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON LeagueItemDTO

data MiniSeriesDTO = MiniSeriesDTO
  { losses   :: Int,
    progress :: String,
    target   :: Int,
    wins     :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON MiniSeriesDTO

type Division = String

type EncryptedPUUID = String

type EncryptedSummonerId = String

type LeagueId = String

type Queue = String

type Tier = String

challengerleaguesByQueue :: Platform -> Queue -> IO (TypedRequest LeagueListDTO)
challengerleaguesByQueue platform queue = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/league/v4/challengerleagues/by-queue/" ++ queue
  return $ TypedRequest request

entriesByPuuid :: Platform -> EncryptedPUUID -> IO (TypedRequest (Set LeagueEntryDTO))
entriesByPuuid platform encryptedPUUID = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/league/v4/entries/by-puuid/" ++ encryptedPUUID
  return $ TypedRequest request

entriesBySummoner :: Platform -> EncryptedSummonerId -> IO (TypedRequest (Set LeagueEntryDTO))
entriesBySummoner platform encryptedSummonerId = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/league/v4/entries/by-summoner/" ++ encryptedSummonerId
  return $ TypedRequest request

entriesBy :: Platform -> Queue -> Tier -> Division -> IO (TypedRequest (Set LeagueEntryDTO))
entriesBy platform queue tier division = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/league/v4/entries/" ++ queue ++ "/" ++ tier ++ "/" ++ division
  return $ TypedRequest request

grandmasterleaguesByQueue :: Platform -> Queue -> IO (TypedRequest LeagueListDTO)
grandmasterleaguesByQueue platform queue = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/league/v4/grandmasterleagues/by-queue/" ++ queue
  return $ TypedRequest request

leagues :: Platform -> LeagueId -> IO (TypedRequest LeagueListDTO)
leagues platform leagueId = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/league/v4/leagues/" ++ leagueId
  return $ TypedRequest request

masterleaguesByQueue :: Platform -> Queue -> IO (TypedRequest LeagueListDTO)
masterleaguesByQueue platform queue = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/league/v4/masterleagues/by-queue/" ++ queue
  return $ TypedRequest request
