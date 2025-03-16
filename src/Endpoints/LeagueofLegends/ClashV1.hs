{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.LeagueofLegends.ClashV1 where

import           Data.Aeson          (FromJSON)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data PlayerDto = PlayerDto
  { summonerId :: String,
    puuid      :: String,
    teamId     :: String,
    {-(Legal values:  UNSELECTED,  FILL,  TOP,  JUNGLE,  MIDDLE,  BOTTOM,  UTILITY)-}
    position   :: String,
    {-(Legal values:  CAPTAIN,  MEMBER)-}
    role       :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlayerDto

data TeamDto = TeamDto
  { id           :: String,
    tournamentId :: Int,
    name         :: String,
    iconId       :: Int,
    tier         :: Int,
    {- | Summoner ID of the team captain.-}
    captain      :: String,
    abbreviation :: String,
    {- | Team members.-}
    players      :: [PlayerDto]
  }
  deriving (Show, Eq, Generic)

instance FromJSON TeamDto

data TournamentDto = TournamentDto
  { id               :: Int,
    themeId          :: Int,
    nameKey          :: String,
    nameKeySecondary :: String,
    {- | Tournament phase.-}
    schedule         :: [TournamentPhaseDto]
  }
  deriving (Show, Eq, Generic)

instance FromJSON TournamentDto

data TournamentPhaseDto = TournamentPhaseDto
  { id               :: Int,
    registrationTime :: Int,
    startTime        :: Int,
    cancelled        :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON TournamentPhaseDto

type Puuid = String

type TeamId = String

type TournamentId = String

playersByPuuid :: Platform -> Puuid -> IO (TypedRequest [PlayerDto])
playersByPuuid platform puuid = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/clash/v1/players/by-puuid/" ++ puuid
  return $ TypedRequest request

teams :: Platform -> TeamId -> IO (TypedRequest TeamDto)
teams platform teamId = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/clash/v1/teams/" ++ teamId
  return $ TypedRequest request

tournaments :: Platform -> IO (TypedRequest [TournamentDto])
tournaments platform = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/clash/v1/tournaments"
  return $ TypedRequest request

tournamentsByTeam :: Platform -> TeamId -> IO (TypedRequest TournamentDto)
tournamentsByTeam platform teamId = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/clash/v1/tournaments/by-team/" ++ teamId
  return $ TypedRequest request

tournamentByTournamentId :: Platform -> TournamentId -> IO (TypedRequest TournamentDto)
tournamentByTournamentId platform tournamentId = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/clash/v1/tournaments/" ++ tournamentId
  return $ TypedRequest request
