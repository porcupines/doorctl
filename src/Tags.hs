{-# LANGUAGE OverloadedStrings #-}
module Tags
  ( ValidTags
  , tagService
  ) where

import Control.Concurrent        (threadDelay)
import Control.Concurrent.Async  (Async, async)
import Control.Concurrent.MVar   (MVar, swapMVar)
import Control.Exception         (onException)
import Control.Monad             (forever, when, void)
import Data.Aeson                (decode, encode)
import Data.ByteString.Lazy      (readFile, writeFile)
import Data.Text                 (Text, unpack)
import Data.Text.Encoding        (encodeUtf8)
import Network.HTTP.Simple       (httpJSON, parseRequest_, addRequestHeader,
                                  getResponseBody)
import Prelude hiding            (readFile, writeFile)
import System.Console.Concurrent (outputConcurrent)
import System.Directory          (doesFileExist, removeFile)

import Config
import Web

type ValidTags = [Text]

tagService :: Config -> MVar ValidTags -> IO (Async ())
tagService cfg vtVar = async $ do
  let cacheFile = unpack . tagCache $ cfg
  cacheExists <- doesFileExist cacheFile
  when cacheExists $ do
    maybeTags <- decode <$> readFile cacheFile
    case maybeTags of
      Just tags -> do
        void $ swapMVar vtVar tags
        outputConcurrent $ "fetched " <> (show . length) tags <> " tags from cache\n"
      Nothing   -> do
        outputConcurrent ("tag cache invalid, removing\n" :: String)
        removeFile cacheFile

  forever httpLoop

  where
    httpLoop = do
      (ts, sig) <- genAPISignature . encodeUtf8 . doorSecret $ cfg
      let initReq = parseRequest_ . unpack . doorNFCUrl $ cfg
          req'    = addRequestHeader "X-Auth-Timestamp" ts initReq
          req     = addRequestHeader "X-Auth-Signature" sig req'

      tags <- onException (do
                            vts <- getResponseBody <$> httpJSON req
                            void $ swapMVar vtVar vts
                            return vts)
                          (do
                            outputConcurrent ("http error\n" :: String)
                            threadDelay 5000000
                            httpLoop)

      outputConcurrent $ "fetched " <> (show . length) (tags :: ValidTags)
                                    <> " valid tags\n"

      writeFile (unpack . tagCache $ cfg) (encode tags)

      threadDelay . (* 1000000) . fromIntegral . nfcRefreshRate $ cfg
