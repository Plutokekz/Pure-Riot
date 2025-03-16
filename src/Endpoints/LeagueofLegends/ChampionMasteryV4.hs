{-# LANGUAGE DeriveGeneric #-}

module Endpoints.LeagueofLegends.ChampionMasteryV4 where

import           Data.Aeson          (FromJSON)
import           Data.Map            (Map)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data ChampionMasteryDto = ChampionMasteryDto
  { {- | Player Universal Unique Identifier. Exact length of 78 characters. (Encrypted)-}
    puuid                        :: String,
    {- | Number of points needed to achieve next level. Zero if player reached maximum champion level for this champion.-}
    championPointsUntilNextLevel :: Int,
    {- | Is chest granted for this champion or not in current season.-}
    chestGranted                 :: Bool,
    {- | Champion ID for this entry.-}
    championId                   :: Int,
    {- | Last time this champion was played by this player - in Unix milliseconds time format.-}
    lastPlayTime                 :: Int,
    {- | Champion level for specified player and champion combination.-}
    championLevel                :: Int,
    {- | Total number of champion points for this player and champion combination - they are used to determine championLevel.-}
    championPoints               :: Int,
    {- | Number of points earned since current level has been achieved.-}
    championPointsSinceLastLevel :: Int,
    markRequiredForNextLevel     :: Int,
    championSeasonMilestone      :: Int,
    nextSeasonMilestone          :: NextSeasonMilestonesDto,
    {- | The token earned for this champion at the current championLevel. When the championLevel is advanced the tokensEarned resets to 0.-}
    tokensEarned                 :: Int,
    milestoneGrades              :: [String]
  }
  deriving (Show, Eq, Generic)

instance FromJSON ChampionMasteryDto

data NextSeasonMilestonesDto = NextSeasonMilestonesDto
  { requireGradeCounts :: Map String Int,
    {- | Reward marks.-}
    rewardMarks        :: Int,
    {- | Bonus.-}
    bonus              :: Bool,
    {- | Reward configuration.-}
    rewardConfig       :: RewardConfigDto
  }
  deriving (Show, Eq, Generic)

instance FromJSON NextSeasonMilestonesDto

data RewardConfigDto = RewardConfigDto
  { {- | Reward value-}
    rewardValue   :: String,
    {- | Reward type-}
    rewardType    :: String,
    {- | Maximun reward-}
    maximumReward :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON RewardConfigDto

type ChampionId = String

type EncryptedPUUID = String

championMasteriesByPuuid :: Platform -> EncryptedPUUID -> IO (TypedRequest [ChampionMasteryDto])
championMasteriesByPuuid platform encryptedPUUID = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/champion-mastery/v4/champion-masteries/by-puuid/" ++ encryptedPUUID
  return $ TypedRequest request

championMasteriesByPuuidByChampion :: Platform -> EncryptedPUUID -> ChampionId -> IO (TypedRequest ChampionMasteryDto)
championMasteriesByPuuidByChampion platform encryptedPUUID championId = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/champion-mastery/v4/champion-masteries/by-puuid/" ++ encryptedPUUID ++ "/by-champion/" ++ championId
  return $ TypedRequest request

championMasteriesByPuuidTop :: Platform -> EncryptedPUUID -> IO (TypedRequest [ChampionMasteryDto])
championMasteriesByPuuidTop platform encryptedPUUID = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/champion-mastery/v4/champion-masteries/by-puuid/" ++ encryptedPUUID ++ "/top"
  return $ TypedRequest request

scoresByPuuid :: Platform -> EncryptedPUUID -> IO (TypedRequest int)
scoresByPuuid platform encryptedPUUID = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/champion-mastery/v4/scores/by-puuid/" ++ encryptedPUUID
  return $ TypedRequest request
