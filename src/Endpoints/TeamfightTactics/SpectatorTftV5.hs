{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoints.TeamfightTactics.SpectatorTftV5 where

import           Data.Aeson          (FromJSON)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data CurrentGameInfo = CurrentGameInfo
  { {-The ID of the game-}
    gameId            :: Int,
    {-The game type-}
    gameType          :: String,
    {-The game start time represented in epoch milliseconds-}
    gameStartTime     :: Int,
    {-The ID of the map-}
    mapId             :: Int,
    {-The amount of time in seconds that has passed since the game started-}
    gameLength        :: Int,
    {-The ID of the platform on which the game is being played-}
    platformId        :: String,
    {-The game mode-}
    gameMode          :: String,
    {-Banned champion information-}
    bannedChampions   :: [BannedChampion],
    {-The queue type (queue types are documented on the Game Constants page)-}
    gameQueueConfigId :: Int,
    {-The observer information-}
    observers         :: Observer,
    {-The participant information-}
    participants      :: [CurrentGameParticipant]
  }
  deriving (Show, Eq, Generic)

instance FromJSON CurrentGameInfo

data BannedChampion = BannedChampion
  { {-The turn during which the champion was banned-}
    pickTurn   :: Int,
    {-The ID of the banned champion-}
    championId :: Int,
    {-The ID of the team that banned the champion-}
    teamId     :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON BannedChampion

  {-Key used to decrypt the spectator grid game data for playback-}
newtype Observer= Observer {encryptionKey :: String}
  deriving (Show, Eq, Generic)

instance FromJSON Observer

data CurrentGameParticipant = CurrentGameParticipant
  { {-The ID of the champion played by this participant-}
    championId               :: Int,
    {-Perks/Runes Reforged Information-}
    perks                    :: Perks,
    {-The ID of the profile icon used by this participant-}
    profileIconId            :: Int,
    {-The team ID of this participant, indicating the participant's team-}
    teamId                   :: Int,
    {-The encrypted summoner ID of this participant-}
    summonerId               :: String,
    {-The encrypted puuid of this participant-}
    puuid                    :: String,
    {-The ID of the first summoner spell used by this participant-}
    spell1Id                 :: Int,
    {-The ID of the second summoner spell used by this participant-}
    spell2Id                 :: Int,
    {-List of Game Customizations-}
    gameCustomizationObjects :: [GameCustomizationObject]
  }
  deriving (Show, Eq, Generic)

instance FromJSON CurrentGameParticipant

data Perks = Perks
  { {-IDs of the perks/runes assigned.-}
    perkIds      :: [Int],
    {-Primary runes path-}
    perkStyle    :: Int,
    {-Secondary runes path-}
    perkSubStyle :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON Perks

data GameCustomizationObject = GameCustomizationObject
  { {-Category identifier for Game Customization-}
    category :: String,
    {-Game Customization content-}
    content  :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON GameCustomizationObject

data FeaturedGames = FeaturedGames
  { {-The list of featured games-}
    gameList              :: [FeaturedGameInfo],
    {-The suggested interval to wait before requesting FeaturedGames again-}
    clientRefreshInterval :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON FeaturedGames

data FeaturedGameInfo = FeaturedGameInfo
  { {-The game mode
             (Legal values:  TFT)-}
    gameMode          :: String,
    {-The amount of time in seconds that has passed since the game started-}
    gameLength        :: Int,
    {-The ID of the map-}
    mapId             :: Int,
    {-The game type
             (Legal values:  MATCHED)-}
    gameType          :: String,
    {-Banned champion information-}
    bannedChampions   :: [BannedChampion],
    {-The ID of the game-}
    gameId            :: Int,
    {-The observer information-}
    observers         :: Observer,
    {-The queue type (queue types are documented on the Game Constants page)-}
    gameQueueConfigId :: Int,
    {-The participant information-}
    participants      :: [Participant],
    {-The ID of the platform on which the game is being played-}
    platformId        :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON FeaturedGameInfo

data Participant = Participant
  { {-The ID of the second summoner spell used by this participant-}
    spell2Id      :: Int,
    {-The ID of the profile icon used by this participant-}
    profileIconId :: Int,
    {-Encrypted summoner ID of this participant-}
    summonerId    :: String,
    {-Encrypted puuid of this participant-}
    puuid         :: String,
    {-The ID of the champion played by this participant-}
    championId    :: Int,
    {-The team ID of this participant, indicating the participant's team-}
    teamId        :: Int,
    {-The ID of the first summoner spell used by this participant-}
    spell1Id      :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON Participant

type EncryptedPUUID = String

v5ActiveGamesByPuuid :: Platform -> EncryptedPUUID -> IO (TypedRequest CurrentGameInfo)
v5ActiveGamesByPuuid platform encryptedPUUID = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/spectator/tft/v5/active-games/by-puuid/" ++ encryptedPUUID
  return $ TypedRequest request

v5FeaturedGames :: Platform -> IO (TypedRequest FeaturedGames)
v5FeaturedGames platform = do
  request <- parseRequest $ platformToUrl platform ++ "/lol/spectator/tft/v5/featured-games"
  return $ TypedRequest request
