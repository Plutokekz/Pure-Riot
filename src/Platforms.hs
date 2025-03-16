module Platforms where

data Platform = BR1 | EUN1 | EUW1 | JP1 | KR | LA1 | LA2 | NA1 | OC1 | TR1 | RU | PH2 | SG2 | TH2 | TW2 | VN2 deriving (Show, Eq)

platformToUrl :: Platform -> String
platformToUrl BR1  = "https://br1.api.riotgames.com"
platformToUrl EUN1 = "https://eun1.api.riotgames.com"
platformToUrl EUW1 = "https://euw1.api.riotgames.com"
platformToUrl JP1  = "https://jp1.api.riotgames.com"
platformToUrl KR   = "https://kr.api.riotgames.com"
platformToUrl LA1  = "https://la1.api.riotgames.com"
platformToUrl LA2  = "https://la2.api.riotgames.com"
platformToUrl NA1  = "https://na1.api.riotgames.com"
platformToUrl OC1  = "https://oc1.api.riotgames.com"
platformToUrl TR1  = "https://tr1.api.riotgames.com"
platformToUrl RU   = "https://ru.api.riotgames.com"
platformToUrl PH2  = "https://ph2.api.riotgames.com"
platformToUrl SG2  = "https://sg2.api.riotgames.com"
platformToUrl TH2  = "https://th2.api.riotgames.com"
platformToUrl TW2  = "https://tw2.api.riotgames.com"
platformToUrl VN2  = "https://vn2.api.riotgames.com"
