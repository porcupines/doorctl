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
import Data.Time.Clock (getCurrentTime)
import Prelude hiding            (readFile, writeFile)
import System.Console.Concurrent (outputConcurrent)
import System.Directory          (doesFileExist, removeFile)

import DoorctlAPI (NFCKeys (..), NFCKey (..))
import Api (fetchNFCKeys)
import Config

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
      tags <- onException (do
                            outputConcurrent ("httpLoop\n" :: String)
                            t <- getCurrentTime
                            outputConcurrent ("getCurrentTime successful\n" :: String)
                            NFCKeys vts <- fetchNFCKeys cfg t
                            outputConcurrent ("fetchNFCKeys successful\n" :: String)
                            void $ swapMVar vtVar (unNFCKey <$> vts)
                            outputConcurrent ("swapMVar successful\n" :: String)
                            return vts)
                          (do
                            outputConcurrent ("http error\n" :: String)
                            threadDelay 5000000
                            httpLoop)

      outputConcurrent $ "fetched " <> (show . length) ((unNFCKey <$> tags) :: ValidTags)
                                    <> " valid tags\n"

      writeFile (unpack . tagCache $ cfg) (encode tags)

      threadDelay . (* 1000000) . fromIntegral . nfcRefreshRate $ cfg
