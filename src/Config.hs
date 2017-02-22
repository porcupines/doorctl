{-# LANGUAGE DeriveGeneric #-}
module Config
  ( Reader(..)
  , Config(..)
  , loadConfig
  ) where

import Dhall
import GHC.Generics (Generic)

data Reader =
  Reader { readerId    :: Text
         , readerDev   :: Text
         , actuatorPin :: Natural
         } deriving (Generic, Show)

data Config =
  Config { doorSecret     :: Text
         , doorNFCUrl     :: Text
         , doorLogUrl     :: Text
         , nfcRefreshRate :: Natural
         , doorHoldTime   :: Natural
         , tagReadTTL     :: Natural
         , tagCache       :: Text
         , watchdogCount  :: Natural
         , watchdogPeriod :: Natural
         , readers        :: Vector Reader
         } deriving (Generic, Show)

instance Interpret Reader
instance Interpret Config

loadConfig :: Text -> IO Config
loadConfig = input auto
