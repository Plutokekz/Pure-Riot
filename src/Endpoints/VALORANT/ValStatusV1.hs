{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.VALORANT.ValStatusV1 where

import           Data.Aeson          (FromJSON)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data PlatformDataDto = PlatformDataDto
  { id           :: String,
    name         :: String,
    locales      :: [String],
    maintenances :: [StatusDto],
    incidents    :: [StatusDto]
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlatformDataDto

data StatusDto = StatusDto
  { id                 :: Int,
    {-(Legal values:  scheduled,  in_progress,  complete)-}
    maintenance_status :: String,
    {-(Legal values:  info,  warning,  critical)-}
    incident_severity  :: String,
    titles             :: [ContentDto],
    updates            :: [UpdateDto],
    created_at         :: String,
    archive_at         :: String,
    updated_at         :: String,
    {-(Legal values: windows, macos, android, ios, ps4, xbone, switch)-}
    platforms          :: [String]
  }
  deriving (Show, Eq, Generic)

instance FromJSON StatusDto

data ContentDto = ContentDto
  { locale  :: String,
    content :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON ContentDto

data UpdateDto = UpdateDto
  { id                :: Int,
    author            :: String,
    publish           :: Bool,
    {-(Legal values: riotclient, riotstatus, game)-}
    publish_locations :: [String],
    translations      :: [ContentDto],
    created_at        :: String,
    updated_at        :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateDto

platformData :: Platform -> IO (TypedRequest PlatformDataDto)
platformData platform = do
  request <- parseRequest $ platformToUrl platform ++ "/val/status/v1/platform-data"
  return $ TypedRequest request
