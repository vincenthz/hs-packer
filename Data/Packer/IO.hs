-- |
-- Module      : Data.Packer.IO
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.Packer.IO
    ( runPackingIO
    , runUnpackingIO
    , tryUnpackingIO
    ) where

import Data.Packer.Internal
import Data.Packer.Unsafe
import Foreign.ForeignPtr
import qualified Control.Exception as E

import Data.ByteArray (ByteArrayAccess(..), ByteArray(..), MemView(..))
import qualified Data.ByteArray as B
import qualified Data.Memory.PtrMethods as B

-- | Unpack a bytestring using a monadic unpack action in the IO monad.
runUnpackingIO :: ByteArrayAccess bytes => bytes -> Unpacking a -> IO a
runUnpackingIO ba action = runUnpackingAt ba 0 (B.length ba) action

-- | Similar to 'runUnpackingIO' but catch exception and return an Either type.
tryUnpackingIO :: ByteArrayAccess bytes => bytes -> Unpacking a -> IO (Either E.SomeException a)
tryUnpackingIO ba action = E.try $ runUnpackingIO ba action

-- | Run packing with a buffer created internally with a monadic action and return the bytestring
runPackingIO :: ByteArray bytes => Int -> Packing a -> IO (a, bytes)
runPackingIO sz action = do
    ((a, size), bytes) <- B.allocRet sz $ \ptr -> runPackingAt ptr sz action
    newBytes <- trimeBytes bytes size
    return (a, newBytes)

trimeBytes :: ByteArray bytes
           => bytes
           -> Int
           -> IO bytes
trimeBytes src size =
    B.create size $ \ptrDst ->
    B.withByteArray src $ \ptrSrc ->
        B.memCopy ptrDst ptrSrc size
