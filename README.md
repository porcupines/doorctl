# doorctl

## Generating a keypair

$ cabal repl
ghci> import Crypto.Sign.Ed25519
ghci> import Data.ByteString.Base64
ghci> (pk, sk) <- createKeypair
ghci> encode (unPublicKey pk)
ghci> encode (unSecretKey sk)

