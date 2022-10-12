{-# LANGUAGE TypeApplications #-}


module Api
  ( logAccessAttempt
  , fetchNFCKeys
  ) where


import Config (Config (doorLogUrl, doorNFCUrl))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (unpack)
import Data.Time.Clock (UTCTime)
import DoorctlAPI (LogAccessAttemptAPI, NFCKey (..), AccessAttemptResult (..), NFCKeys (..), Signature (..), FetchNFCKeysAPI)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client (runClientM, client, ClientEnv, mkClientEnv, parseBaseUrl)
import System.Console.Concurrent (outputConcurrent)


emptySignature :: Signature
emptySignature = Signature mempty


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
  res <- runClientM
    (client (Proxy @LogAccessAttemptAPI)
      time result key emptySignature) -- TODO: signature
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
  res <- runClientM
    (client (Proxy @FetchNFCKeysAPI)
      time emptySignature) -- TODO: signature
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
