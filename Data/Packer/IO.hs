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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B (ByteString(..), mallocByteString, toForeignPtr)
import Foreign.ForeignPtr
import qualified Control.Exception as E

-- | Unpack a bytestring using a monadic unpack action in the IO monad.
runUnpackingIO :: ByteString -> UnpackingStrict a -> IO a
runUnpackingIO bs action = runUnpackingAt fptr o len action
  where (fptr,o,len) = B.toForeignPtr bs

-- | Similar to 'runUnpackingIO' but catch exception and return an Either type.
tryUnpackingIO :: ByteString -> UnpackingStrict a -> IO (Either E.SomeException a)
tryUnpackingIO bs action = E.try $ runUnpackingIO bs action

-- | Run packing with a buffer created internally with a monadic action and return the bytestring
runPackingIO :: Int -> PackingStrict a -> IO (a, ByteString)
runPackingIO sz action = createUptoN sz $ \ptr -> runPackingAt ptr sz action
    where -- copy of bytestring createUptoN as it's only been added 2012-09
          createUptoN l f = do fp <- B.mallocByteString l
                               (a, l') <- withForeignPtr fp $ \p -> f p
                               return $! (,) a $! B.PS fp 0 l'

