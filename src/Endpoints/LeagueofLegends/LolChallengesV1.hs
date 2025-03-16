{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.LeagueofLegends.LolChallengesV1 where

import           Data.Aeson          (FromJSON)
import           Data.Map            (Map)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data ApexPlayerInfoDto = ApexPlayerInfoDto
  { puuid    :: String,
    value    :: Double,
    position :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ApexPlayerInfoDto

data ChallengeConfigInfoDto = ChallengeConfigInfoDto
  { id             :: Int,
    localizedNames :: Map String (Map String String),
    state          :: State,
    tracking       :: Tracking,
    startTimestamp :: Int,
    endTimestamp   :: Int,
    leaderboard    :: Bool,
    thresholds     :: Map String Double
  }
  deriving (Show, Eq, Generic)

instance FromJSON ChallengeConfigInfoDto

newtype State = State String
  deriving (Show, Eq, Generic)

instance FromJSON State

newtype Tracking = Tracking String
  deriving (Show, Eq, Generic)

instance FromJSON Tracking

data ChallengePoints = ChallengePoints
  { level      :: String,
    current    :: Int,
    max        :: Int,
    percentile :: Float
  }
  deriving (Show, Eq, Generic)

instance FromJSON ChallengePoints

data ChallengeInfo = ChallengeInfo
  { challengeId  :: Int,
    percentile   :: Float,
    level        :: String,
    value        :: Int,
    achievedTime :: Integer
  }
  deriving (Show, Eq, Generic)

instance FromJSON ChallengeInfo

data PlayerClientPreferences = PlayerClientPreferences
  { bannerAccent :: String,
    title        :: String,
    challengeIds :: [Int]
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlayerClientPreferences

data PlayerInfoDto = PlayerInfoDto
  { challenges     :: [ChallengeInfo],
    preferences    :: PlayerClientPreferences,
    totalPoints    :: ChallengePoints,
    categoryPoints :: Map String ChallengePoints
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlayerInfoDto

type ChallengeId = String

type Level = String

type Puuid = String

challengesConfig :: Platform -> IO (TypedRequest [ChallengeConfigInfoDto])
challengesConfig platform = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/challenges/v1/challenges/config"
  return $ TypedRequest request

challengesPercentiles :: Platform -> IO (TypedRequest (Map Int (Map Int (Map Level Double))))
challengesPercentiles platform = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/challenges/v1/challenges/percentiles"
  return $ TypedRequest request

challengesConfigbyId :: Platform -> ChallengeId -> IO (TypedRequest ChallengeConfigInfoDto)
challengesConfigbyId platform challengeId = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/challenges/v1/challenges/" ++ challengeId ++ "/config"
  return $ TypedRequest request

challengesLeaderboardsByLevel :: Platform -> ChallengeId -> Level -> IO (TypedRequest [ApexPlayerInfoDto])
challengesLeaderboardsByLevel platform challengeId level = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/challenges/v1/challenges/" ++ challengeId ++ "/leaderboards/by-level/" ++ level
  return $ TypedRequest request

challengesPercentilesById :: Platform -> ChallengeId -> IO (TypedRequest (Map Level Double))
challengesPercentilesById platform challengeId = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/challenges/v1/challenges/" ++ challengeId ++ "/percentiles"
  return $ TypedRequest request

playerData :: Platform -> Puuid -> IO (TypedRequest PlayerInfoDto)
playerData platform puuid = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/challenges/v1/player-data/" ++ puuid
  return $ TypedRequest request
