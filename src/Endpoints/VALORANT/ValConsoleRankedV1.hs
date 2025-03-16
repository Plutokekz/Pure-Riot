{-# LANGUAGE DeriveGeneric #-}

module Endpoints.VALORANT.ValConsoleRankedV1 where

import           Data.Aeson          (FromJSON)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data LeaderboardDto = LeaderboardDto
  { {-The shard for the given leaderboard.-}
    shard        :: String,
    {-The act id for the given leaderboard. Act ids can be found using the val-content API.-}
    actId        :: String,
    {-The total number of players in the leaderboard.-}
    totalPlayers :: Int,
    players      :: [PlayerDto]
  }
  deriving (Show, Eq, Generic)

instance FromJSON LeaderboardDto

data PlayerDto = PlayerDto
  { {-This field may be omitted if the player has been anonymized.-}
    puuid           :: String,
    {-This field may be omitted if the player has been anonymized.-}
    gameName        :: String,
    {-This field may be omitted if the player has been anonymized.-}
    tagLine         :: String,
    leaderboardRank :: Int,
    rankedRating    :: Int,
    numberOfWins    :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlayerDto

type ActId = String

v1LeaderboardsByAct :: Platform -> ActId -> IO (TypedRequest LeaderboardDto)
v1LeaderboardsByAct platform actId = do
  request <- parseRequest $ platformToUrl platform ++ "/val/console/ranked/v1/leaderboards/by-act/" ++ actId
  return $ TypedRequest request
