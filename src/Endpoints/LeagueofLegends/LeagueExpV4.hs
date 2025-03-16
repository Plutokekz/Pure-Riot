{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.LeagueofLegends.LeagueExpV4 where

import           Data.Aeson          (FromJSON)
import           Data.Set            (Set)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data LeagueEntryDTO = LeagueEntryDTO
  { leagueId     :: String,
    {-Player's summonerId (Encrypted)-}
    summonerId   :: String,
    {-Player's encrypted puuid.-}
    puuid        :: String,
    queueType    :: String,
    tier         :: String,
    {-The player's division within a tier.-}
    rank         :: String,
    leaguePoints :: Int,
    {-Winning team on Summoners Rift. First placement in Teamfight Tactics.-}
    wins         :: Int,
    {-Losing team on Summoners Rift. Second through eighth placement in Teamfight Tactics.-}
    losses       :: Int,
    hotStreak    :: Bool,
    veteran      :: Bool,
    freshBlood   :: Bool,
    inactive     :: Bool,
    miniSeries   :: MiniSeriesDTO
  }
  deriving (Show, Eq, Generic)

instance FromJSON LeagueEntryDTO

data MiniSeriesDTO = MiniSeriesDTO
  { losses   :: Int,
    progress :: String,
    target   :: Int,
    wins     :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON MiniSeriesDTO

type Division = String

type Queue = String

type Tier = String

entries :: Platform -> Queue -> Tier -> Division -> IO (TypedRequest (Set LeagueEntryDTO))
entries platform queue tier division = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/league-exp/v4/entries/" ++ queue ++ "/" ++ tier ++ "/" ++ division
  return $ TypedRequest request
