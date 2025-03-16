module Regions where

data Region = AMERICAS | ESPORTS | ASIA | EUROPE
  deriving (Show, Eq)

regionToUrl :: Region -> String
regionToUrl AMERICAS = "https://americas.api.riotgames.com"
regionToUrl EUROPE   = "https://europe.api.riotgames.com"
regionToUrl ASIA     = "https://asia.api.riotgames.com"
regionToUrl ESPORTS  = "https://esports.api.riotgames.com"
