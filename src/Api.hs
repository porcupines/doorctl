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


emptySignature :: Signature
emptySignature = Signature mempty


logAccessAttempt
  :: Config
  -> UTCTime
  -> AccessAttemptResult
  -> NFCKey
  -> IO ()
logAccessAttempt config time result key = do
  env <- accessAttemptClientEnv config
  res <- runClientM
    (client (Proxy @LogAccessAttemptAPI)
      time result key emptySignature) -- TODO: signature
    env
  case res of
    Right _ -> pure ()
    Left err -> putStrLn $
      "error logging access attempt: "
      <> show err


accessAttemptClientEnv :: Config -> IO ClientEnv
accessAttemptClientEnv cfg = do
  mgr <- newManager defaultManagerSettings
  pure . mkClientEnv mgr 
    . fromMaybe (error ("malformed doorLogUrl"))
    $ parseBaseUrl (unpack (doorLogUrl cfg))


fetchNFCKeys
  :: Config -> UTCTime -> IO NFCKeys
fetchNFCKeys cfg time = do
  env <- fetchNFCKeysClientEnv cfg
  res <- runClientM
    (client (Proxy @FetchNFCKeysAPI)
      time emptySignature) -- TODO: signature
    env
  case res of
    Right keys -> pure keys
    Left err -> do
      putStrLn $
        "error fetching NFC keys: "
        <> show err
      pure (NFCKeys [])


fetchNFCKeysClientEnv :: Config -> IO ClientEnv
fetchNFCKeysClientEnv cfg = do
  mgr <- newManager defaultManagerSettings
  pure . mkClientEnv mgr
    . fromMaybe (error "malformed doorNFCUrl")
    $ parseBaseUrl (unpack (doorNFCUrl cfg))
