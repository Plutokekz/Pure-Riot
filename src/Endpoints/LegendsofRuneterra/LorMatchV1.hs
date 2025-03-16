{-# LANGUAGE DeriveGeneric #-}

module Endpoints.LegendsofRuneterra.LorMatchV1 where

import           Data.Aeson          (FromJSON)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data MatchDto = MatchDto
  { {-Match metadata.-}
    metadata :: MetadataDto,
    {-Match info.-}
    info     :: InfoDto
  }
  deriving (Show, Eq, Generic)

instance FromJSON MatchDto

data MetadataDto = MetadataDto
  { {-Match data version.-}
    data_version :: String,
    {-Match id.-}
    match_id     :: String,
    {-A list of participant PUUIDs.-}
    participants :: [String]
  }
  deriving (Show, Eq, Generic)

instance FromJSON MetadataDto

data InfoDto = InfoDto
  { {-(Legal values:  Constructed,  Expeditions,  Tutorial)-}
    game_mode           :: String,
    {-(Legal values:  Ranked,  Normal,  AI,  Tutorial,  VanillaTrial,  Singleton,  StandardGauntlet)-}
    game_type           :: String,
    game_start_time_utc :: String,
    game_version        :: String,
    {-(Legal values:  standard,  eternal)-}
    game_format         :: String,
    players             :: [PlayerDto],
    {-Total turns taken by both players.-}
    total_turn_count    :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON InfoDto

data PlayerDto = PlayerDto
  { puuid         :: String,
    deck_id       :: String,
    {-Code for the deck played. Refer to LOR documentation for details on deck codes.-}
    deck_code     :: String,
    factions      :: [String],
    game_outcome  :: String,
    {-The order in which the players took turns.-}
    order_of_play :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlayerDto

type MatchId = String

type Puuid = String

matchesByPuuidIds :: Platform -> Puuid -> IO (TypedRequest [string])
matchesByPuuidIds platform puuid = do
  request <- parseRequest $ platformToUrl platform ++ "/lor/match/v1/matches/by-puuid/" ++ puuid ++ "/ids"
  return $ TypedRequest request

matches :: Platform -> MatchId -> IO (TypedRequest MatchDto)
matches platform matchId = do
  request <- parseRequest $ platformToUrl platform ++ "/lor/match/v1/matches/" ++ matchId
  return $ TypedRequest request
