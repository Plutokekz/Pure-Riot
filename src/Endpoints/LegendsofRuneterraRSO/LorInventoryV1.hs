{-# LANGUAGE DeriveGeneric #-}

module Endpoints.LegendsofRuneterraRSO.LorInventoryV1 where

import           Data.Aeson          (FromJSON)
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (parseRequest)
import           Platforms           (Platform (..), platformToUrl)
import           PureRiot            (TypedRequest (TypedRequest))

data CardDto = CardDto
  { code  :: String,
    count :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON CardDto

cardsMe :: Platform -> IO (TypedRequest [CardDto])
cardsMe platform = do
  request <- parseRequest $ platformToUrl platform ++ "/lor/inventory/v1/cards/me"
  return $ TypedRequest request
