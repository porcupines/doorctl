module Main where

import Control.Concurrent.Async  (waitAnyCatchCancel)
import Control.Concurrent.MVar   (newMVar)
import Data.Text                 (pack)
import System.Console.Concurrent (withConcurrentOutput, outputConcurrent)
import System.Environment        (getArgs)
import System.IO                 (BufferMode(..), stdout, hSetBuffering)

import Config
import Exit
import Reader
import Tags

main :: IO ()
main = withConcurrentOutput $ do
  hSetBuffering stdout NoBuffering

  [cfgFileStr] <- getArgs
  config <- loadConfig . pack $ cfgFileStr

  vtVar         <- newMVar []
  tagThread     <- tagService config vtVar
  readerThreads <- readerService config vtVar

  (a, r) <- waitAnyCatchCancel $ tagThread : readerThreads
  let threadThatExited = if a == tagThread
                           then "tag"
                           else "reader"

  case r of
    Right _ -> outputConcurrent $ "the " <> threadThatExited
                                         <> " thread has exited successfully\n"
    Left ex -> do
      outputConcurrent $ "the " <> threadThatExited
                                <> " thread has thrown an exception: "
                                <> show ex
                                <> "\n"
      c_exit 1
