module GroupNote.Random
    ( genToken
    ) where

import Crypto.Random.AESCtr (makeSystem, AESRNG)
import Crypto.Random.API (cprgGenBytes)
import Data.ByteString (ByteString)
import Data.ByteString.Base32 (encode)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Tuple (swap)

cprgRef :: IO (IORef AESRNG)
cprgRef = makeSystem >>= newIORef

genToken :: Int -> IO ByteString
genToken len = do
    cprg <- cprgRef
    encode <$> atomicModifyIORef' cprg (swap . cprgGenBytes len)
