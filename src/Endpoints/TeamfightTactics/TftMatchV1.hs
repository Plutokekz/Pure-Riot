{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.TeamfightTactics.TftMatchV1 where

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
  { {-Unix timestamp.-}
    game_datetime  :: Int,
    {-Game length in seconds.-}
    game_length    :: Double,
    {-Game variation key. Game variations documented in TFT static data.-}
    game_variation :: String,
    {-Game client version.-}
    game_version   :: String,
    participants   :: [ParticipantDto],
    {-Please refer to the League of Legends documentation.-}
    queue_id       :: Int,
    {-Teamfight Tactics set number.-}
    tft_set_number :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON InfoDto

data CompanionDto = CompanionDto
  { skin_ID    :: Int,
    content_ID :: String,
    species    :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON CompanionDto

data ParticipantDto = ParticipantDto
  { {-Participant's companion.-}
    companion               :: CompanionDto,
    {-Gold left after participant was eliminated.-}
    gold_left               :: Int,
    {-The round the participant was eliminated in. Note: If the player was eliminated in stage 2-1 their last_round would be 5.-}
    last_round              :: Int,
    {-Participant Little Legend level. Note: This is not the number of active units.-}
    level                   :: Int,
    {-Participant placement upon elimination.-}
    placement               :: Int,
    {-Number of players the participant eliminated.-}
    players_eliminated      :: Int,
    puuid                   :: String,
    riotIdGameName          :: String,
    riotIdTagline           :: String,
    {-The Double of seconds before the participant was eliminated.-}
    time_eliminated         :: Double,
    {-Damage the participant dealt to other players.-}
    total_damage_to_players :: Int,
    {-A complete list of traits for the participant's active units.-}
    traits                  :: [TraitDto],
    {-A list of active units for the participant.-}
    units                   :: [UnitDto]
  }
  deriving (Show, Eq, Generic)

instance FromJSON ParticipantDto

data TraitDto = TraitDto
  { {-Trait name.-}
    name         :: String,
    {-Number of units with this trait.-}
    num_units    :: Int,
    {-Current style for this trait. (0 = No style, 1 = Bronze, 2 = Silver, 3 = Gold, 4 = Chromatic)-}
    style        :: Int,
    {-Current active tier for the trait.-}
    tier_current :: Int,
    {-Total tiers for the trait.-}
    tier_total   :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON TraitDto

data UnitDto = UnitDto
  { {-A list of the unit's items. Please refer to the Teamfight Tactics documentation for item ids.-}
    items        :: [Int],
    {-This field was introduced in patch 9.22 with data_version 2.-}
    character_id :: String,
    {-If a unit is chosen as part of the Fates set mechanic, the chosen trait will be indicated by this field. Otherwise this field is excluded from the response.-}
    chosen       :: String,
    {-Unit name. This field is often left blank.-}
    name         :: String,
    {-Unit rarity. This doesn't equate to the unit cost.-}
    rarity       :: Int,
    {-Unit tier.-}
    tier         :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON UnitDto

type MatchId = String

type Puuid = String

matchesByPuuidIds :: Platform -> Puuid -> IO (TypedRequest [string])
matchesByPuuidIds platform puuid = do
  request <- parseRequest $ platformToUrl platform ++ "/tft/match/v1/matches/by-puuid/" ++ puuid ++ "/ids"
  return $ TypedRequest request

matches :: Platform -> MatchId -> IO (TypedRequest MatchDto)
matches platform matchId = do
  request <- parseRequest $ platformToUrl platform ++ "/tft/match/v1/matches/" ++ matchId
  return $ TypedRequest request
