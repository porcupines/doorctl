{-# LANGUAGE OverloadedStrings #-}
module Reader where

import Control.Concurrent           (threadDelay)
import Control.Concurrent.Async     (Async, async, concurrently_)
import Control.Concurrent.MVar      (MVar, readMVar)
import Control.Monad                (forever, void, forM, when)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (runResourceT, resourceForkWith,
                                     allocate)
import qualified Data.ByteString.Lazy as BL (fromStrict)
import qualified Data.ByteString.Base16 as B16 (encode)
import Data.List                    (nub)
import Data.Maybe                   (fromMaybe)
import Data.Monoid                  ((<>))
import qualified Data.Text.Lazy     as LT (unpack, toUpper, toStrict)
import qualified Data.Text.Lazy.Encoding as LE (decodeUtf8)
import qualified Data.Vector        as V (toList)
import Numeric.Natural              (Natural)
import System.Console.Concurrent    (withConcurrentOutput, outputConcurrent)

import qualified Bindings.NFC       as NFC

import Config
import GPIO
import Log
import Tags

lockDoor :: PinHandle -> IO ()
lockDoor = flip setPin PinHigh

unlockDoor :: PinHandle -> IO ()
unlockDoor = flip setPin PinLow

cycleDoor :: PinHandle -> Natural -> IO ()
cycleDoor ph unlockTime = withConcurrentOutput $ do
  unlockDoor ph
  outputConcurrent $ "[" <> show ph <> "] door unlocked\n"

  threadDelay . (* 1000000) . fromIntegral $ unlockTime

  lockDoor ph
  outputConcurrent $ "[" <> show ph <> "] door locked\n"

readerService :: Config -> MVar ValidTags -> IO [Async ()]
readerService cfg vtVar = withConcurrentOutput $ do
  ctx <- NFC.initialize

  runResourceT $ do
    -- A single door may be controlled by multiple readers. We must only
    -- allocate them once.
    let pinsToAlloc = nub (map actuatorPin . V.toList . readers $ cfg)

    allocatedPinMap <- forM pinsToAlloc $ \pinNum -> do
      (_, allocatedPin) <- allocate (allocPin pinNum) freePin

      liftIO $ do
        lockDoor allocatedPin
        outputConcurrent $ "[" <> show allocatedPin <> "] door locked (startup)\n"

      return (pinNum, allocatedPin)

    forM (V.toList . readers $ cfg) $ \r -> resourceForkWith async . liftIO $ do
      outputConcurrent $ "[" <> (LT.toStrict . readerId) r <> "] opening reader\n"
      -- Open the reader device and set it to initiator mode.
      maybeDev <- NFC.open ctx $ (LT.unpack . readerDev) r
      let dev = fromMaybe (error "error opening device") maybeDev
      void . NFC.initiatorInit $ dev

      let pin = fromMaybe (error "couldn't find pin in allocatedPinMap")
                          (lookup (actuatorPin r) allocatedPinMap)

      forever $ readerLoop dev pin (readerId r)

  where
    readerLoop dev pin rid = do
      let nfcMod = NFC.NFCModulation NFC.NmtIso14443a NFC.Nbr106
      maybeInfo <- NFC.initiatorPollTarget dev [nfcMod] 7 10

      case maybeInfo of
        Just (NFC.NFCTargetISO14443a info) -> do
          let nfcValue = LT.toUpper . LE.decodeUtf8 . BL.fromStrict . B16.encode . NFC.iso14443aAbtUid $ info

          outputConcurrent $ "[" <> LT.toStrict rid <> "] read tag: " <> LT.toStrict nfcValue <> "\n"

          vt <- readMVar vtVar
          let authorized = nfcValue `elem` vt
              logEntry   = LogEntry nfcValue authorized rid

          concurrently_
            (when authorized $ cycleDoor pin $ doorHoldTime cfg)
            (submitLogEntry cfg logEntry)

        _ -> return ()

      threadDelay . (* 1000000) . fromIntegral . tagReadTTL $ cfg
