{-# LANGUAGE OverloadedStrings #-}
module Log
  ( LogEntry(..)
  , submitLogEntry
  ) where

import Control.Concurrent  (forkIO, threadDelay)
import Control.Exception   (onException)
import Control.Monad       (void)
import Data.Aeson          (ToJSON(..), (.=), object)
import Data.Text           (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Foreign.C.Types     (CTime(..))
import System.Posix        (EpochTime)

import DoorctlAPI (AccessAttemptResult (AccessGranted, AccessNotGranted), NFCKey (..))

import Api (logAccessAttempt)
import Config

data LogEntry =
  LogEntry { leNfcValue :: Text
           , leGranted  :: Bool
           , leReader   :: Text
           , leLoggedAt :: EpochTime
           }

instance ToJSON LogEntry where
  toJSON le = object [ "nfc_value" .= leNfcValue le
                     , "granted"   .= leGranted  le
                     , "reader"    .= leReader   le
                     , "logged_at" .= epochToInt (leLoggedAt le)
                     ]

epochToInt :: EpochTime -> Int
epochToInt (CTime t) = fromIntegral t

epochTimeToUTCTime :: EpochTime -> UTCTime
epochTimeToUTCTime =
  posixSecondsToUTCTime . fromIntegral . fromEnum

submitLogEntry :: Config -> LogEntry -> IO ()
submitLogEntry cfg le = void . forkIO $ go
  where
    signature = todo

    go = onException
      (logAccessAttempt cfg
        (epochTimeToUTCTime (leLoggedAt le))
        (if leGranted le then AccessGranted
         else AccessNotGranted)
        (NFCKey (leNfcValue le))
        signature)

      (threadDelay (5 * 1000000) >> go)


todo :: a
todo = todo
