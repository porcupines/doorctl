module GPIO
  ( PinHandle
  , PinState(..)
  , setPin
  , allocPin
  , freePin
  ) where 

import Numeric.Natural  (Natural)

newtype PinHandle = PinHandle { pinHandle :: Natural }

instance Show PinHandle where
  show = show . pinHandle

data PinState = PinHigh | PinLow

instance Show PinState where
  show PinHigh = "1"
  show PinLow  = "0"

pinPath :: PinHandle -> FilePath
pinPath ph = "/sys/class/gpio/gpio" <> show ph

setPin :: PinHandle -> PinState -> IO ()
setPin ph ps = writeFile (pinPath ph <> "/value") $ show ps

allocPin :: Natural -> IO PinHandle
allocPin p = do
  let ph = PinHandle p
  putStrLn "allocPin"
--   pinExported <- doesDirectoryExist . pinPath $ ph
--   unless pinExported $
--     writeFile "/sys/class/gpio/export" $ show p
--   writeFile (pinPath ph <> "/direction") "out"
  return ph

freePin :: PinHandle -> IO ()
freePin _ph = do
  putStrLn "freePin"
  -- pinExported <- doesDirectoryExist . pinPath $ ph
  -- when pinExported $
  --   writeFile "/sys/class/gpio/unexport" $ show ph
