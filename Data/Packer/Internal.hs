{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Packer.Internal
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Internal of packer which is a simple state monad that hold
-- a memory pointer and a size of the memory pointed.
--
module Data.Packer.Internal
    ( Memory(..)
    -- * exceptions
    , OutOfBoundUnpacking(..)
    , OutOfBoundPacking(..)
    , HoleInPacking(..)
    , IsolationNotFullyConsumed(..)
    -- * util
    , unsafeDoIO
    ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Data
import Data.Word
import Control.Exception (Exception, throwIO, try, SomeException)
import Control.Monad.Trans
import Control.Applicative (Alternative(..), Applicative(..), (<$>), (<*>))
import Control.Concurrent.MVar
import Control.Monad (when)
import Data.Packer.Family

import System.IO.Unsafe

#if __GLASGOW_HASKELL__ > 704
unsafeDoIO = unsafeDupablePerformIO
#else
unsafeDoIO = unsafePerformIO
#endif

-- | Represent a memory block with a ptr as beginning
data Memory = Memory {-# UNPACK #-} !(Ptr Word8)
                     {-# UNPACK #-} !Int

-- | Exception when trying to put bytes out of the memory bounds.
data OutOfBoundPacking = OutOfBoundPacking Int -- position relative to the end
                                           Int -- number of bytes requested
    deriving (Show,Eq,Data,Typeable)

-- | Exception when trying to finalize the packing monad that still have holes open.
data HoleInPacking = HoleInPacking Int
    deriving (Show,Eq,Data,Typeable)

-- | Exception when trying to get bytes out of the memory bounds.
data OutOfBoundUnpacking = OutOfBoundUnpacking Int -- position
                                               Int -- number of bytes requested
    deriving (Show,Eq,Data,Typeable)

-- | Exception when isolate doesn't consume all the bytes passed in the sub unpacker
data IsolationNotFullyConsumed = IsolationNotFullyConsumed Int -- number of bytes isolated
                                                           Int -- number of bytes not consumed
    deriving (Show,Eq,Data,Typeable)

instance Exception OutOfBoundPacking
instance Exception HoleInPacking
instance Exception OutOfBoundUnpacking
instance Exception IsolationNotFullyConsumed
