{-# LANGUAGE DeriveGeneric #-}

module Endpoints.TeamfightTacticsRSO.TftSummonerV1 where

import           Data.Aeson          (FromJSON)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data SummonerDTO = SummonerDTO
  { {- | Encrypted account ID. Max length 56 characters.-}
    accountId     :: String,
    {- | ID of the summoner icon associated with the summoner.-}
    profileIconId :: Int,
    {- | Date summoner was last modified specified as epoch milliseconds. The following events will update this timestamp: summoner name change, summoner level change, or profile icon change.-}
    revisionDate  :: Int,
    {- | Encrypted summoner ID. Max length 63 characters.-}
    id            :: String,
    {- | Encrypted PUUID. Exact length of 78 characters.-}
    puuid         :: String,
    {- | Summoner level associated with the summoner.-}
    summonerLevel :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON SummonerDTO

type EncryptedAccountId = String

type EncryptedPUUID = String

type EncryptedSummonerId = String

summonersByAccount :: Platform -> EncryptedAccountId -> IO (TypedRequest SummonerDTO)
summonersByAccount platform encryptedAccountId = do
  request <- parseRequest $ platformToUrl platform ++ "/tft/summoner/v1/summoners/by-account/" ++ encryptedAccountId
  return $ TypedRequest request

summonersByPuuid :: Platform -> EncryptedPUUID -> IO (TypedRequest SummonerDTO)
summonersByPuuid platform encryptedPUUID = do
  request <- parseRequest $ platformToUrl platform ++ "/tft/summoner/v1/summoners/by-puuid/" ++ encryptedPUUID
  return $ TypedRequest request

summonersMe :: Platform -> IO (TypedRequest SummonerDTO)
summonersMe platform = do
  request <- parseRequest $ platformToUrl platform ++ "/tft/summoner/v1/summoners/me"
  return $ TypedRequest request

summoners :: Platform -> EncryptedSummonerId -> IO (TypedRequest SummonerDTO)
summoners platform encryptedSummonerId = do
  request <- parseRequest $ platformToUrl platform ++ "/tft/summoner/v1/summoners/" ++ encryptedSummonerId
  return $ TypedRequest request
