module Web
  ( genAPISignature
  ) where

import Data.ByteArray        (convert)
import Data.ByteString       (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Base64 as B64 (encode)
import Crypto.Hash           (SHA256)
import Crypto.MAC.HMAC       (HMAC, hmac)
import System.Posix          (epochTime)

type Timestamp = ByteString
type Signature = ByteString

genAPISignature :: ByteString -> IO (Timestamp, Signature)
genAPISignature secret = do
  ts <- pack . show <$> epochTime
  let sig = B64.encode . convert $ (hmac secret ts :: HMAC SHA256)
  return (ts, sig)
