{-# LANGUAGE TypeApplications #-}


module Api
  ( logAccessAttempt
  ) where


import Config (Config, doorLogUrl)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (unpack)
import Data.Time.Clock (UTCTime)
import DoorctlAPI (LogAccessAttemptAPI, NFCKey (..), AccessAttemptResult (..))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client (runClientM, client, ClientEnv, mkClientEnv, parseBaseUrl)


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
      time result key)
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
