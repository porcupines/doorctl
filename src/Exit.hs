{-# LANGUAGE ForeignFunctionInterface #-}
module Exit where

foreign import ccall "stdlib.h exit" c_exit :: Int -> IO ()
