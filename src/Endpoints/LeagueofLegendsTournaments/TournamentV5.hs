{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.LeagueofLegendsTournaments.TournamentV5 where

import           Data.Aeson          (FromJSON)
import           Data.Set            (Set)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

newtype LobbyEventV5DTOWrapper
    = LobbyEventV5DTOWrapper {eventList :: [LobbyEventV5DTO]}
    deriving (Show, Eq, Generic)

instance FromJSON LobbyEventV5DTOWrapper

data LobbyEventV5DTO = LobbyEventV5DTO
  { {- | Timestamp from the event-}
    timestamp :: String,
    {- | The type of event that was triggered-}
    eventType :: String,
    {- | The puuid that triggered the event (Encrypted)-}
    puuid     :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON LobbyEventV5DTO

data TournamentCodeV5DTO = TournamentCodeV5DTO
  { {- | The tournament code.-}
    code         :: String,
    {- | The spectator mode for the tournament code game.-}
    spectators   :: String,
    {- | The lobby name for the tournament code game.-}
    lobbyName    :: String,
    {- | The metadata for tournament code.-}
    metaData     :: String,
    {- | The password for the tournament code game.-}
    password     :: String,
    {- | The team size for the tournament code game.-}
    teamSize     :: Int,
    {- | The provider's ID.-}
    providerId   :: Int,
    {- | The pick mode for tournament code game.-}
    pickType     :: String,
    {- | The tournament's ID.-}
    tournamentId :: Int,
    {- | The tournament code's ID.-}
    id           :: Int,
    {- | The tournament code's region.
             (Legal values:  BR,  EUNE,  EUW,  JP,  LAN,  LAS,  NA,  OCE,  PBE,  RU,  TR,  KR,  PH,  SG,  TH,  TW,  VN)-}
    region       :: String,
    {- | The game map for the tournament code game-}
    map          :: String,
    {- | The puuids of the participants (Encrypted)-}
    participants :: Set String
  }
  deriving (Show, Eq, Generic)

instance FromJSON TournamentCodeV5DTO

data TournamentGamesV5 = TournamentGamesV5
  { winningTeam :: [TournamentTeamV5],
    losingTeam  :: [TournamentTeamV5],
    {- | Tournament Code-}
    shortCode   :: String,
    {- | Metadata for the TournamentCode-}
    metaData    :: String,
    gameId      :: Int,
    gameName    :: String,
    gameType    :: String,
    {- | Game Map ID-}
    gameMap     :: Int,
    gameMode    :: String,
    {- | Region of the game-}
    region      :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON TournamentGamesV5
  {- | Player Unique UUID (Encrypted)-}
newtype TournamentTeamV5
    = TournamentTeamV5 {puuid :: String}
    deriving (Show, Eq, Generic)

instance FromJSON TournamentTeamV5

type TournamentCode = String

codes :: Platform -> TournamentCode -> IO (TypedRequest TournamentCodeV5DTO)
codes platform tournamentCode = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/tournament/v5/codes/" ++ tournamentCode
  return $ TypedRequest request

gamesByCode :: Platform -> TournamentCode -> IO (TypedRequest (Set TournamentGamesV5))
gamesByCode platform tournamentCode = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/tournament/v5/games/by-code/" ++ tournamentCode
  return $ TypedRequest request

lobbyEventsByCode :: Platform -> TournamentCode -> IO (TypedRequest LobbyEventV5DTOWrapper)
lobbyEventsByCode platform tournamentCode = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/tournament/v5/lobby-events/by-code/" ++ tournamentCode
  return $ TypedRequest request
