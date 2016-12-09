{-# LANGUAGE OverloadedStrings #-}
module Tags
  ( ValidTags
  , tagService
  ) where

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.MVar  (MVar, swapMVar)
import Control.Exception        (onException)
import Control.Monad            (forever, when, void)
import Data.Aeson               (decode, encode)
import Data.ByteString.Lazy     (toStrict, readFile, writeFile)
import Data.Monoid              ((<>))
import Data.Text.Lazy           (Text, unpack)
import Data.Text.Lazy.Encoding  (encodeUtf8)
import Network.HTTP.Simple      (httpJSON, parseRequest_, addRequestHeader,
                                 getResponseBody)
import Prelude hiding           (readFile, writeFile)
import System.Directory         (doesFileExist, removeFile)

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
        putStrLn $ "fetched " <> (show . length) tags <> " tags from cache"
      Nothing   -> do
        putStrLn "tag cache invalid, removing"
        removeFile cacheFile

  forever httpLoop

  where
    httpLoop = do
      (ts, sig) <- genAPISignature . toStrict . encodeUtf8 . doorSecret $ cfg
      let initReq = parseRequest_ . unpack . doorNFCUrl $ cfg
          req'    = addRequestHeader "X-Auth-Timestamp" ts initReq
          req     = addRequestHeader "X-Auth-Signature" sig req'

      tags <- onException (do
                            vts <- getResponseBody <$> httpJSON req
                            void $ swapMVar vtVar vts
                            return vts)
                          (do
                            putStrLn "http error"
                            threadDelay 5000000
                            httpLoop)

      putStrLn $ "fetched " <> (show . length) (tags :: ValidTags)
                            <> " valid tags"

      writeFile (unpack . tagCache $ cfg) (encode tags)

      threadDelay . (* 1000000) . fromIntegral . nfcRefreshRate $ cfg
