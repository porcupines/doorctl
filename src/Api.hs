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
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client (runClientM, client, ClientEnv, mkClientEnv, parseBaseUrl)
import System.Console.Concurrent (outputConcurrent)


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
  outputConcurrent "logAccessAttempt\n"
  env <- accessAttemptClientEnv config
  outputConcurrent "got accessAttemptClientEnv\n"
  let sig = sign (privateKey config) (time, result, key)
  res <- runClientM
    (client (Proxy @LogAccessAttemptAPI)
      time result key sig)
    env
  outputConcurrent "called LogAccessAttemptAPI\n"
  case res of
    Right _ -> pure ()
    Left err -> outputConcurrent $
      "error logging access attempt: "
      <> show err <> "\n"


accessAttemptClientEnv :: Config -> IO ClientEnv
accessAttemptClientEnv cfg = do
  mgr <- newManager defaultManagerSettings
  pure . mkClientEnv mgr 
    . fromMaybe (error ("malformed doorLogUrl"))
    $ parseBaseUrl (unpack (doorLogUrl cfg))


fetchNFCKeys
  :: Config -> UTCTime -> IO NFCKeys
fetchNFCKeys cfg time = do
  outputConcurrent "fetchNFCKeys\n"
  env <- fetchNFCKeysClientEnv cfg
  outputConcurrent "got fetchNFCKeysClientEnv\n"
  let sig = sign (privateKey cfg) time
  res <- runClientM
    (client (Proxy @FetchNFCKeysAPI) time sig)
    env
  outputConcurrent "called FetchNFCKeysAPI\n"
  case res of
    Right keys -> pure keys
    Left err -> do
      outputConcurrent $
        "error fetching NFC keys: \n"
        <> show err <> "\n"
      pure (NFCKeys [])


fetchNFCKeysClientEnv :: Config -> IO ClientEnv
fetchNFCKeysClientEnv cfg = do
  outputConcurrent "fetchNFCKeysClientEnv\n"
  mgr <- newManager defaultManagerSettings
  outputConcurrent "newManager was successful\n"
  case parseBaseUrl (unpack (doorNFCUrl cfg)) of
    Just baseUrl ->
      pure (mkClientEnv mgr baseUrl)
    Nothing -> do
      outputConcurrent "parseBaseUrl was unsuccessful :-(\n"
      pure (error "foo")
