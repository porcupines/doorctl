{-# LANGUAGE OverloadedStrings #-}
module Reader where

import Control.Concurrent           (threadDelay)
import Control.Concurrent.Async     (Async, async)
import Control.Concurrent.MVar      (MVar, readMVar)
import Control.Monad                (forever, void, forM, when)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (runResourceT, resourceForkWith,
                                     allocate)
import qualified Data.ByteString.Base16 as B16 (encode)
import Data.IORef                   (IORef, newIORef, atomicModifyIORef')
import Data.List                    (nub)
import Data.Maybe                   (fromMaybe)
import Data.Monoid                  ((<>))
import Data.Text                    (unpack, toUpper)
import Data.Text.Encoding           (decodeUtf8)
import qualified Data.Vector        as V (toList)
import Numeric.Natural              (Natural)
import System.Console.Concurrent    (outputConcurrent)
import System.Posix                 (epochTime)

import qualified Bindings.NFC       as NFC

import Config
import GPIO
import Log
import Tags
import Watchdog

lockDoor :: PinHandle -> IO ()
lockDoor = flip setPin PinHigh

unlockDoor :: PinHandle -> IO ()
unlockDoor = flip setPin PinLow

cycleDoor :: PinHandle -> Natural -> IO ()
cycleDoor ph unlockTime = do
  unlockDoor ph
  outputConcurrent $ "[" <> show ph <> "] door unlocked\n"

  threadDelay . (* 1000000) . fromIntegral $ unlockTime

  lockDoor ph
  outputConcurrent $ "[" <> show ph <> "] door locked\n"

resetCountdown :: Config -> IORef Countdown -> IO ()
resetCountdown cfg countdownRef = do
  let newValue = fromIntegral . watchdogCount $ cfg
  atomicModifyIORef' countdownRef $ const (newValue, ())

readerService :: Config -> MVar ValidTags -> IO [Async ()]
readerService cfg vtVar = do
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
      -- Start the watchdog service before doing anything else
      countdownRef <- newIORef . fromIntegral . watchdogCount $ cfg
      void $ watchdogService cfg (readerId r) countdownRef

      outputConcurrent $ "[" <> readerId r <> "] opening reader\n"

      -- Open the reader device and set it to initiator mode.
      maybeDev <- NFC.open ctx $ (unpack . readerDev) r
      let dev = fromMaybe (error "error opening device") maybeDev
      void . NFC.initiatorInit $ dev

      let pin = fromMaybe (error "couldn't find pin in allocatedPinMap")
                          (lookup (actuatorPin r) allocatedPinMap)

      forever $ readerLoop dev pin r countdownRef

  where
    readerLoop dev pin reader countdownRef = do
      let nfcMod = NFC.NFCModulation NFC.NmtIso14443a NFC.Nbr106
      maybeInfo <- NFC.initiatorPollTarget dev [nfcMod] 3 1

      resetCountdown cfg countdownRef

      case maybeInfo of
        Just (NFC.NFCTargetISO14443a info) -> do
          let nfcValue = toUpper
                       . decodeUtf8
                       . B16.encode
                       . NFC.iso14443aAbtUid
                       $ info

          ts <- epochTime

          outputConcurrent $ "[" <> readerId reader <> "] read tag: " <> nfcValue <> "\n"

          vt <- readMVar vtVar
          let authorized = nfcValue `elem` vt ||
                           (guestAccess reader && nfcValue `elem` (V.toList . guestNFCValues $ cfg))
              logEntry   = LogEntry nfcValue authorized (readerId reader) ts

          when authorized $ cycleDoor pin $ doorHoldTime cfg

          submitLogEntry cfg logEntry -- forkIO called in here

          threadDelay . (* 1000000) . fromIntegral . tagReadTTL $ cfg

        _ -> return ()
