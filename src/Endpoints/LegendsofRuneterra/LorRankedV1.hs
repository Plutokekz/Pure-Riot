{-# LANGUAGE DeriveGeneric #-}

module Endpoints.LegendsofRuneterra.LorRankedV1 where

import           Data.Aeson          (FromJSON)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

{- | A list of players in Master tier.-}
newtype LeaderboardDto
    = LeaderboardDto {players :: [PlayerDto]}
    deriving (Show, Eq, Generic)

instance FromJSON LeaderboardDto

data PlayerDto = PlayerDto
  { name :: String,
    rank :: Int,
    {- | League points.-}
    lp   :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlayerDto

leaderboards :: Platform -> IO (TypedRequest LeaderboardDto)
leaderboards platform = do
  request <- parseRequest $ platformToUrl platform ++ "/lor/ranked/v1/leaderboards"
  return $ TypedRequest request
