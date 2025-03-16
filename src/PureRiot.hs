module PureRiot (createManager, fetchTyped, TypedRequest (TypedRequest)) where

import           Control.Exception.Base    (try)
import           Data.Aeson                (FromJSON, eitherDecode)
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.CaseInsensitive      as CI
import           Data.Data                 (Proxy (..), typeRep)
import           Data.Typeable             (Typeable)
import           Network.HTTP.Client       (HttpException, Manager,
                                            ManagerSettings (managerModifyRequest),
                                            Request (requestHeaders),
                                            Response (responseBody, responseStatus),
                                            httpLbs, newManager)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header (hAccept, hUserAgent)
import           Network.HTTP.Types.Status (Status (..))

newtype TypedRequest a = TypedRequest Request deriving (Show)

fetch :: (FromJSON a, Typeable a) => Data.Data.Proxy a -> Manager -> Request -> IO (Either String a)
fetch proxy manager request = do
  response <- try (httpLbs request manager) :: IO (Either HttpException (Response LBS.ByteString))
  case response of
    Left err -> return $ Left $ "HTTP request failed: " ++ show err
    Right res -> do
      case responseStatus res of
        (Status 200 _) -> do
          let body = responseBody res
          case eitherDecode body of
            Right user -> return $ Right user
            Left err -> return $ Left $ "Failed to decode type: " ++ show (typeRep proxy) ++ err
        (Status 429 msg) -> return $ Left $ "To many requests: " ++ show msg ++ " " ++ BS.unpack (LBS.toStrict $ responseBody res)
        (Status st msg) -> return $ Left $ "Error status code " ++ show st ++ " " ++ BS.unpack msg

fetchTyped :: (FromJSON a, Typeable a) => Manager -> TypedRequest a -> IO (Either String a)
fetchTyped manager (TypedRequest req) = fetch (Data.Data.Proxy :: Data.Data.Proxy a) manager req

createManager :: String -> IO Manager
createManager apiKey = do
  let settings =
        tlsManagerSettings
          { managerModifyRequest = \req ->
              return $
                req
                  { requestHeaders =
                      requestHeaders req
                        ++ [ (hUserAgent, BS.pack "Pure Riot"),
                             (hAccept, BS.pack "application/json"),
                             (CI.mk $ BS.pack "X-Riot-Token", BS.pack apiKey)
                           ]
                  }
          }
  newManager settings
