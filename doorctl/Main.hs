module Main where

import Control.Concurrent.Async (waitAnyCatch)
import Control.Concurrent.MVar  (newMVar)
import Control.Monad            (void)
import Data.Text.Lazy           (pack)
import System.Environment       (getArgs)
import System.IO                (BufferMode(..), stdout, hSetBuffering)

import Config
import Reader
import Tags

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  [cfgFileStr] <- getArgs
  config <- loadConfig . pack $ cfgFileStr

  vtVar         <- newMVar []
  tagThread     <- tagService config vtVar
  readerThreads <- readerService config vtVar

  void . waitAnyCatch $ tagThread : readerThreads
