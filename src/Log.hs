{-# LANGUAGE OverloadedStrings #-}
module Log
  ( LogEntry(..)
  , submitLogEntry
  ) where

import Control.Monad           (void)
import Data.Aeson              (ToJSON(..), (.=), object)
import Data.ByteString.Lazy    (toStrict)
import Data.Monoid             ((<>))
import Data.Text.Lazy          (Text, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Simple     (parseRequest_, addRequestHeader, setRequestBodyJSON,
                                httpLBS)

import Config
import Web

data LogEntry =
  LogEntry { leNfcValue :: Text
           , leGranted  :: Bool
           , leReader   :: Text
           }

instance ToJSON LogEntry where
  toJSON le = object [ "nfc_value" .= leNfcValue le
                     , "granted"   .= leGranted  le
                     , "reader"    .= leReader   le
                     ]

submitLogEntry :: Config -> LogEntry -> IO ()
submitLogEntry cfg le = do
  (ts, sig)   <- genAPISignature . toStrict . encodeUtf8 . doorSecret $ cfg
  let initReq = parseRequest_ $ "POST " <> (unpack . doorLogUrl) cfg
      req''   = addRequestHeader "X-Auth-Timestamp" ts initReq
      req'    = addRequestHeader "X-Auth-Signature" sig req''
      req     = setRequestBodyJSON le req'

  void . httpLBS $ req
