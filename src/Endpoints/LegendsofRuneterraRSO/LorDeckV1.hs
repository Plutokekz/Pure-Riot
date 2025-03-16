{-# LANGUAGE DeriveGeneric #-}

module Endpoints.LegendsofRuneterraRSO.LorDeckV1 where

import           Data.Aeson          (FromJSON)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data DeckDto = DeckDto
  { id   :: String,
    name :: String,
    code :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON DeckDto

decksMe :: Platform -> IO (TypedRequest [DeckDto])
decksMe platform = do
  request <- parseRequest $ platformToUrl platform ++ "/lor/deck/v1/decks/me"
  return $ TypedRequest request
