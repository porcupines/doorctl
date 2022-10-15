{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Reader(..)
  , Config(..)
  , loadConfig
  ) where

import Dhall

data Reader =
  Reader { readerId    :: Text
         , readerDev   :: Text
         , actuatorPin :: Natural
         , guestAccess :: Bool
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
         , guestNFCValues :: Vector Text
         , readers        :: Vector Reader
         } deriving (Generic, Show)

instance FromDhall Reader
instance FromDhall Config

loadConfig :: Text -> IO Config
loadConfig = input auto
