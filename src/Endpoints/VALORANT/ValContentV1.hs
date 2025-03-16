{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.VALORANT.ValContentV1 where

import           Data.Aeson          (FromJSON)
import           Data.Map            (Map)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data ContentDto = ContentDto
  { version      :: String,
    characters   :: [ContentItemDto],
    maps         :: [ContentItemDto],
    chromas      :: [ContentItemDto],
    skins        :: [ContentItemDto],
    skinLevels   :: [ContentItemDto],
    equips       :: [ContentItemDto],
    gameModes    :: [ContentItemDto],
    sprays       :: [ContentItemDto],
    sprayLevels  :: [ContentItemDto],
    charms       :: [ContentItemDto],
    charmLevels  :: [ContentItemDto],
    playerCards  :: [ContentItemDto],
    playerTitles :: [ContentItemDto],
    acts         :: [ActDto]
  }
  deriving (Show, Eq, Generic)

instance FromJSON ContentDto

data ContentItemDto = ContentItemDto
  { name           :: String,
    {- | This field is excluded from the response when a locale is set-}
    localizedNames :: LocalizedNamesDto,
    id             :: String,
    assetName      :: String,
    {- | This field is only included for maps and game modes. These values are used in the match response.-}
    assetPath      :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON ContentItemDto

newtype LocalizedNamesDto = LocalizedNamesDto (Map String String)
  deriving (Show, Eq, Generic)

instance FromJSON LocalizedNamesDto

data ActDto = ActDto
  { name           :: String,
    {- | This field is excluded from the response when a locale is set-}
    localizedNames :: LocalizedNamesDto,
    id             :: String,
    isActive       :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON ActDto

contents :: Platform -> IO (TypedRequest ContentDto)
contents platform = do
  request <- parseRequest $ platformToUrl platform ++ "/val/content/v1/contents"
  return $ TypedRequest request
