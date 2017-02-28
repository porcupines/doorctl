{-# LANGUAGE OverloadedStrings #-}
module Log
  ( LogEntry(..)
  , submitLogEntry
  ) where

import Control.Concurrent      (forkIO, threadDelay)
import Control.Exception       (onException)
import Control.Monad           (void)
import Data.Aeson              (ToJSON(..), (.=), object)
import Data.ByteString.Lazy    (toStrict)
import Data.Monoid             ((<>))
import Data.Text.Lazy          (Text, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Foreign.C.Types         (CTime(..))
import Network.HTTP.Simple     (parseRequest_, addRequestHeader, setRequestBodyJSON,
                                httpLBS)
import System.Posix            (EpochTime)

import Config
import Web

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

submitLogEntry :: Config -> LogEntry -> IO ()
submitLogEntry cfg le = void . forkIO $ go
  where
    go = onException
      (do
        (ts, sig)   <- genAPISignature . toStrict . encodeUtf8 . doorSecret $ cfg
        let initReq = parseRequest_ $ "POST " <> (unpack . doorLogUrl) cfg
            req''   = addRequestHeader "X-Auth-Timestamp" ts initReq
            req'    = addRequestHeader "X-Auth-Signature" sig req''
            req     = setRequestBodyJSON le req'

        void . httpLBS $ req)

      (threadDelay (5 * 1000000) >> go)
