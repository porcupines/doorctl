{-# LANGUAGE OverloadedStrings #-}
module Watchdog where

import Control.Concurrent        (threadDelay)
import Control.Concurrent.Async  (Async, async)
import Control.Monad             (forever, when)
import Data.IORef                (IORef, atomicModifyIORef')
import Data.Monoid               ((<>))
import Data.Text.Lazy            (Text)
import System.Console.Concurrent (outputConcurrent)

import Config
import Exit

type Countdown = Int

watchdogService :: Config -> Text -> IORef Countdown -> IO (Async ())
watchdogService cfg name countdownRef = async . forever $ do
  countdown <- atomicModifyIORef' countdownRef $ \c -> (c-1, c)

  when (countdown < 0) $ do
    outputConcurrent $ "[" <> name <> "] service stalled, aborting\n"
    c_exit 1

  threadDelay . (* 1000000) . fromIntegral . watchdogPeriod $ cfg
