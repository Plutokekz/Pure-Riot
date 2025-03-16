{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.VALORANT.ValConsoleMatchV1 where

import           Data.Aeson          (FromJSON)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data MatchDto = MatchDto
  { matchInfo    :: MatchInfoDto,
    players      :: [PlayerDto],
    coaches      :: [CoachDto],
    teams        :: [TeamDto],
    roundResults :: [RoundResultDto]
  }
  deriving (Show, Eq, Generic)

instance FromJSON MatchDto

data MatchInfoDto = MatchInfoDto
  { matchId            :: String,
    mapId              :: String,
    gameLengthMillis   :: Int,
    gameStartMillis    :: Int,
    provisioningFlowId :: String,
    isCompleted        :: Bool,
    customGameName     :: String,
    queueId            :: String,
    gameMode           :: String,
    isRanked           :: Bool,
    seasonId           :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON MatchInfoDto

data PlayerDto = PlayerDto
  { puuid           :: String,
    gameName        :: String,
    tagLine         :: String,
    teamId          :: String,
    partyId         :: String,
    characterId     :: String,
    stats           :: PlayerStatsDto,
    competitiveTier :: Int,
    playerCard      :: String,
    playerTitle     :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlayerDto

data PlayerStatsDto = PlayerStatsDto
  { score          :: Int,
    roundsPlayed   :: Int,
    kills          :: Int,
    deaths         :: Int,
    assists        :: Int,
    playtimeMillis :: Int,
    abilityCasts   :: AbilityCastsDto
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlayerStatsDto

data AbilityCastsDto = AbilityCastsDto
  { grenadeCasts  :: Int,
    ability1Casts :: Int,
    ability2Casts :: Int,
    ultimateCasts :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON AbilityCastsDto

data CoachDto = CoachDto
  { puuid  :: String,
    teamId :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON CoachDto

data TeamDto = TeamDto
  { {- | This is an arbitrary string. Red and Blue in bomb modes. The puuid of the player in deathmatch.-}
    teamId       :: String,
    won          :: Bool,
    roundsPlayed :: Int,
    roundsWon    :: Int,
    {- | Team points scored. Number of kills in deathmatch.-}
    numPoints    :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON TeamDto

data RoundResultDto = RoundResultDto
  { roundNum              :: Int,
    roundResult           :: String,
    roundCeremony         :: String,
    winningTeam           :: String,
    {- | PUUID of player-}
    bombPlanter           :: String,
    {- | PUUID of player-}
    bombDefuser           :: String,
    plantRoundTime        :: Int,
    plantPlayerLocations  :: [PlayerLocationsDto],
    plantLocation         :: LocationDto,
    plantSite             :: String,
    defuseRoundTime       :: Int,
    defusePlayerLocations :: [PlayerLocationsDto],
    defuseLocation        :: LocationDto,
    playerStats           :: [PlayerRoundStatsDto],
    roundResultCode       :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON RoundResultDto

data PlayerLocationsDto = PlayerLocationsDto
  { puuid       :: String,
    viewRadians :: Double,
    location    :: LocationDto
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlayerLocationsDto

data LocationDto = LocationDto
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON LocationDto

data PlayerRoundStatsDto = PlayerRoundStatsDto
  { puuid   :: String,
    kills   :: [KillDto],
    damage  :: [DamageDto],
    score   :: Int,
    economy :: EconomyDto,
    ability :: AbilityDto
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlayerRoundStatsDto

data KillDto = KillDto
  { timeSinceGameStartMillis  :: Int,
    timeSinceRoundStartMillis :: Int,
    {- | PUUID-}
    killer                    :: String,
    {- | PUUID-}
    victim                    :: String,
    victimLocation            :: LocationDto,
    {- | List of PUUIDs-}
    assistants                :: [String],
    playerLocations           :: [PlayerLocationsDto],
    finishingDamage           :: FinishingDamageDto
  }
  deriving (Show, Eq, Generic)

instance FromJSON KillDto

data FinishingDamageDto = FinishingDamageDto
  { damageType          :: String,
    damageItem          :: String,
    isSecondaryFireMode :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON FinishingDamageDto

data DamageDto = DamageDto
  { {- | PUUID-}
    receiver  :: String,
    damage    :: Int,
    legshots  :: Int,
    bodyshots :: Int,
    headshots :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON DamageDto

data EconomyDto = EconomyDto
  { loadoutValue :: Int,
    weapon       :: String,
    armor        :: String,
    remaining    :: Int,
    spent        :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON EconomyDto

data AbilityDto = AbilityDto
  { grenadeEffects  :: String,
    ability1Effects :: String,
    ability2Effects :: String,
    ultimateEffects :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON AbilityDto

data MatchlistDto = MatchlistDto
  { puuid   :: String,
    history :: [MatchlistEntryDto]
  }
  deriving (Show, Eq, Generic)

instance FromJSON MatchlistDto

data MatchlistEntryDto = MatchlistEntryDto
  { matchId             :: String,
    gameStartTimeMillis :: Int,
    queueId             :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON MatchlistEntryDto

data RecentMatchesDto = RecentMatchesDto
  { currentTime :: Int,
    {- | A list of recent match ids.-}
    matchIds    :: [String]
  }
  deriving (Show, Eq, Generic)

instance FromJSON RecentMatchesDto

type MatchId = String

type Puuid = String

type Queue = String

v1Matches :: Platform -> MatchId -> IO (TypedRequest MatchDto)
v1Matches platform matchId = do
  request <- parseRequest $ platformToUrl platform ++ "/val/match/console/v1/matches/" ++ matchId
  return $ TypedRequest request

v1MatchlistsByPuuid :: Platform -> Puuid -> IO (TypedRequest MatchlistDto)
v1MatchlistsByPuuid platform puuid = do
  request <- parseRequest $ platformToUrl platform ++ "/val/match/console/v1/matchlists/by-puuid/" ++ puuid
  return $ TypedRequest request

v1RecentMatchesByQueue :: Platform -> Queue -> IO (TypedRequest RecentMatchesDto)
v1RecentMatchesByQueue platform queue = do
  request <- parseRequest $ platformToUrl platform ++ "/val/match/console/v1/recent-matches/by-queue/" ++ queue
  return $ TypedRequest request
