{-# LANGUAGE DeriveGeneric #-}

module Endpoints.LeagueofLegends.ChampionV3 where

import           Data.Aeson          (FromJSON)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data ChampionInfo = ChampionInfo
  { maxNewPlayerLevel            :: Int,
    freeChampionIdsForNewPlayers :: [Int],
    freeChampionIds              :: [Int]
  }
  deriving (Show, Eq, Generic)

instance FromJSON ChampionInfo

championRotations :: Platform -> IO (TypedRequest ChampionInfo)
championRotations platform = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/platform/v3/champion-rotations"
  return $ TypedRequest request
