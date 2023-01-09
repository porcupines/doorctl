{-# LANGUAGE TypeApplications #-}


module Api
  ( logAccessAttempt
  , fetchNFCKeys
  ) where


import DoorctlAPI (PrivateSigningKey (..))
import Config (Config (doorLogUrl, doorNFCUrl, doorSecret))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import DoorctlAPI (LogAccessAttemptAPI, NFCKey (..), AccessAttemptResult (..), NFCKeys (..), FetchNFCKeysAPI, sign)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (runClientM, client, ClientEnv, mkClientEnv, parseBaseUrl)


privateKey :: Config -> PrivateSigningKey
privateKey =
  PrivateSigningKey .
  encodeUtf8 .
  doorSecret


logAccessAttempt
  :: Config
  -> UTCTime
  -> AccessAttemptResult
  -> NFCKey
  -> IO ()
logAccessAttempt config time result key = do
  env <- accessAttemptClientEnv config
  let sig = sign (privateKey config) (time, result, key)
  res <- runClientM
    (client (Proxy @LogAccessAttemptAPI)
      time result key sig)
    env
  case res of
    Right _ -> pure ()
    Left err -> putStrLn $
      "error logging access attempt: "
      <> show err


accessAttemptClientEnv :: Config -> IO ClientEnv
accessAttemptClientEnv cfg = do
  mgr <- newManager tlsManagerSettings
  pure . mkClientEnv mgr 
    . fromMaybe (error ("malformed doorLogUrl"))
    $ parseBaseUrl (unpack (doorLogUrl cfg))


fetchNFCKeys
  :: Config -> UTCTime -> IO (Maybe NFCKeys)
fetchNFCKeys cfg time = do
  env <- fetchNFCKeysClientEnv cfg
  let sig = sign (privateKey cfg) time
  res <- runClientM
    (client (Proxy @FetchNFCKeysAPI) time sig)
    env
  case res of
    Right keys -> pure . pure $ keys
    Left err -> do
      putStrLn $
        "error fetching NFC keys: "
        <> show err
      pure Nothing


fetchNFCKeysClientEnv :: Config -> IO ClientEnv
fetchNFCKeysClientEnv cfg = do
  mgr <- newManager tlsManagerSettings
  pure . mkClientEnv mgr
    . fromMaybe (error "malformed doorNFCUrl")
    $ parseBaseUrl (unpack (doorNFCUrl cfg))
