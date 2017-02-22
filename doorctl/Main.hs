module Main where

import Control.Concurrent.Async  (waitAnyCatch)
import Control.Concurrent.MVar   (newMVar)
import Data.Monoid               ((<>))
import Data.Text.Lazy            (pack)
import System.Console.Concurrent (withConcurrentOutput, outputConcurrent)
import System.Environment        (getArgs)
import System.IO                 (BufferMode(..), stdout, hSetBuffering)

import Config
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

  (a, r) <- waitAnyCatch $ tagThread : readerThreads
  let threadThatExited = if a == tagThread
                           then "tag"
                           else "reader"

  case r of
    Right _ -> outputConcurrent $ "the " <> threadThatExited
                                         <> " thread has exited successfully\n"
    Left ex -> outputConcurrent $ "the " <> threadThatExited
                                         <> " thread has thrown an exception: "
                                         <> show ex
                                         <> "\n"
