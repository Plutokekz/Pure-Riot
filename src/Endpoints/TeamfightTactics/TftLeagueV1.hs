{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.TeamfightTactics.TftLeagueV1 where

import           Data.Aeson          (FromJSON)
import           Data.Set            (Set)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data LeagueEntryDTO = LeagueEntryDTO
  { {-Player Universal Unique Identifier. Exact length of 78 characters. (Encrypted)-}
    puuid        :: String,
    {-Not included for the RANKED_TFT_TURBO queueType.-}
    leagueId     :: String,
    {-Player's encrypted summonerId.-}
    summonerId   :: String,
    queueType    :: String,
    {-Only included for the RANKED_TFT_TURBO queueType.
             (Legal values:  ORANGE,  PURPLE,  BLUE,  GREEN,  GRAY)-}
    ratedTier    :: String,
    {-Only included for the RANKED_TFT_TURBO queueType.-}
    ratedRating  :: Int,
    {-Not included for the RANKED_TFT_TURBO queueType.-}
    tier         :: String,
    {-The player's division within a tier. Not included for the RANKED_TFT_TURBO queueType.-}
    rank         :: String,
    {-Not included for the RANKED_TFT_TURBO queueType.-}
    leaguePoints :: Int,
    {-First placement.-}
    wins         :: Int,
    {-Second through eighth placement.-}
    losses       :: Int,
    {-Not included for the RANKED_TFT_TURBO queueType.-}
    hotStreak    :: Bool,
    {-Not included for the RANKED_TFT_TURBO queueType.-}
    veteran      :: Bool,
    {-Not included for the RANKED_TFT_TURBO queueType.-}
    freshBlood   :: Bool,
    {-Not included for the RANKED_TFT_TURBO queueType.-}
    inactive     :: Bool,
    {-Not included for the RANKED_TFT_TURBO queueType.-}
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
    {-First placement.-}
    wins         :: Int,
    miniSeries   :: MiniSeriesDTO,
    inactive     :: Bool,
    veteran      :: Bool,
    hotStreak    :: Bool,
    rank         :: String,
    leaguePoints :: Int,
    {-Second through eighth placement.-}
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

data TopRatedLadderEntryDto = TopRatedLadderEntryDto
  { summonerId                   :: String,
    {-(Legal values:  ORANGE,  PURPLE,  BLUE,  GREEN,  GRAY)-}
    ratedTier                    :: String,
    ratedRating                  :: Int,
    {-First placement.-}
    wins                         :: Int,
    previousUpdateLadderPosition :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON TopRatedLadderEntryDto

type Division = String

type LeagueId = String

type Queue = String

type SummonerId = String

type Tier = String

challenger :: Platform -> IO (TypedRequest LeagueListDTO)
challenger platform = do
  request <- parseRequest $ platformToUrl platform ++ "/tft/league/v1/challenger"
  return $ TypedRequest request

entriesBySummoner :: Platform -> SummonerId -> IO (TypedRequest (Set LeagueEntryDTO))
entriesBySummoner platform summonerId = do
  request <- parseRequest $ platformToUrl platform ++ "/tft/league/v1/entries/by-summoner/" ++ summonerId
  return $ TypedRequest request

entriesbyTierAndDivision :: Platform -> Tier -> Division -> IO (TypedRequest (Set LeagueEntryDTO))
entriesbyTierAndDivision platform tier division = do
  request <- parseRequest $ platformToUrl platform ++ "/tft/league/v1/entries/" ++ tier ++ "/" ++ division
  return $ TypedRequest request

grandmaster :: Platform -> IO (TypedRequest LeagueListDTO)
grandmaster platform = do
  request <- parseRequest $ platformToUrl platform ++ "/tft/league/v1/grandmaster"
  return $ TypedRequest request

leagues :: Platform -> LeagueId -> IO (TypedRequest LeagueListDTO)
leagues platform leagueId = do
  request <- parseRequest $ platformToUrl platform ++ "/tft/league/v1/leagues/" ++ leagueId
  return $ TypedRequest request

master :: Platform -> IO (TypedRequest LeagueListDTO)
master platform = do
  request <- parseRequest $ platformToUrl platform ++ "/tft/league/v1/master"
  return $ TypedRequest request

ratedLaddersTop :: Platform -> Queue -> IO (TypedRequest [TopRatedLadderEntryDto])
ratedLaddersTop platform queue = do
  request <- parseRequest $ platformToUrl platform ++ "/tft/league/v1/rated-ladders/" ++ queue ++ "/top"
  return $ TypedRequest request
