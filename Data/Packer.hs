-- |
-- Module      : Data.Packer
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Simple packing module.
--
-- This is a tradeoff between a more pure / builder (binary, cereal, builder)
-- and direct access to Storable or pointer manipulation
--
{-# LANGUAGE CPP #-}
module Data.Packer
    (
    -- * Types
      Packing
    , Unpacking
    , OutOfBoundUnpacking(..)
    , OutOfBoundPacking(..)
    , IsolationNotFullyConsumed(..)
    , Hole
    -- * Main methods
    , runUnpacking
    , tryUnpacking
    , runPacking
    , runPackingRes
    -- * Unpacking functions
    , unpackSkip
    , unpackSetPosition
    , unpackGetPosition
    , getWord8
    , getWord16
    , getWord16LE
    , getWord16BE
    , getWord32
    , getWord32LE
    , getWord32BE
    , getWord64
    , getWord64LE
    , getWord64BE
    , getBytes
    , getBytesWhile
    , getRemaining
    , getStorable
    , getFloat32LE
    , getFloat32BE
    , getFloat64LE
    , getFloat64BE
    , isolate
    -- * Packing functions
    , packGetPosition
    , putWord8
    , putHoleWord8
    , putWord16
    , putWord16LE
    , putWord16BE
    , putHoleWord16
    , putHoleWord16LE
    , putHoleWord16BE
    , putWord32
    , putWord32LE
    , putWord32BE
    , putHoleWord32
    , putHoleWord32LE
    , putHoleWord32BE
    , putWord64
    , putWord64LE
    , putWord64BE
    , putHoleWord64
    , putHoleWord64LE
    , putHoleWord64BE
    , putBytes
    , putStorable
    , putFloat32LE
    , putFloat32BE
    , putFloat64LE
    , putFloat64BE
    , fillHole
    ) where

import Control.Applicative
import Data.Packer.Internal
import Data.Packer.Unsafe
import Data.Packer.IO
import Data.Packer.IEEE754
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word
import Foreign.Storable
import System.IO.Unsafe
import qualified Control.Exception as E

import Data.ByteArray (ByteArray(..), ByteArrayAccess(..))
import qualified Data.ByteArray as B
import qualified Data.Memory.PtrMethods as B
import Data.Memory.Endian

#if __GLASGOW_HASKELL__ > 704
unsafeDoIO = unsafeDupablePerformIO
#else
unsafeDoIO = unsafePerformIO
#endif

-- | Peek and do an action on the result. just for convenience
{-# INLINE peekAnd #-}
peekAnd :: Storable a => (a -> b) -> Ptr a -> IO b
peekAnd f p = f <$> peek p

-- | Skip bytes
unpackSkip :: Int -> Unpacking ()
unpackSkip n = unpackCheckAct n (\_ -> return ())

-- | Get a Word8
getWord8 :: Unpacking Word8
getWord8 = unpackCheckAct 1 peek
{-# INLINE getWord8 #-}

-- | Get a Word16 in the host endianess.
--
-- It's recommended to use an explicit endianness (LE or BE)
-- when unserializing format.
getWord16 :: Unpacking Word16
getWord16 = unpackCheckAct 2 (peek . castPtr)
{-# INLINE getWord16 #-}

-- | Get a Word16 serialized in little endian.
getWord16LE :: Unpacking Word16
getWord16LE = unpackCheckAct 2 (peekAnd fromLE . castPtr)
{-# INLINE getWord16LE #-}

-- | Get a Word16 serialized in big endian.
getWord16BE :: Unpacking Word16
getWord16BE = unpackCheckAct 2 (peekAnd fromBE . castPtr)
{-# INLINE getWord16BE #-}

-- | Get a Word32 in the host endianess.
--
-- It's recommended to use an explicit endianness (LE or BE)
-- when unserializing format.
getWord32 :: Unpacking Word32
getWord32 = unpackCheckAct 4 (peek . castPtr)
{-# INLINE getWord32 #-}

-- | Get a Word32 serialized in little endian.
getWord32LE :: Unpacking Word32
getWord32LE = unpackCheckAct 4 (peekAnd fromLE . castPtr)
{-# INLINE getWord32LE #-}

-- | Get a Word32 serialized in big endian.
getWord32BE :: Unpacking Word32
getWord32BE = unpackCheckAct 4 (peekAnd fromBE . castPtr)
{-# INLINE getWord32BE #-}

-- | Get a Word64 in the host endianess.
--
-- It's recommended to use an explicit endianness (LE or BE)
-- when unserializing format.
getWord64 :: Unpacking Word64
getWord64 = unpackCheckAct 8 (peek . castPtr)
{-# INLINE getWord64 #-}

-- | Get a Word64 serialized in little endian.
getWord64LE :: Unpacking Word64
getWord64LE = unpackCheckAct 8 (peekAnd fromLE . castPtr)
{-# INLINE getWord64LE #-}

-- | Get a Word64 serialized in big endian.
getWord64BE :: Unpacking Word64
getWord64BE = unpackCheckAct 8 (peekAnd fromBE . castPtr)
{-# INLINE getWord64BE #-}

-- | Read a Float in little endian IEEE-754 format
getFloat32LE :: Unpacking Float
getFloat32LE = wordToFloat <$> getWord32LE

-- | Read a Float in big endian IEEE-754 format
getFloat32BE :: Unpacking Float
getFloat32BE = wordToFloat <$> getWord32BE

-- | Read a Double in little endian IEEE-754 format
getFloat64LE :: Unpacking Double
getFloat64LE = wordToDouble <$> getWord64LE

-- | Read a Double in big endian IEEE-754 format
getFloat64BE :: Unpacking Double
getFloat64BE = wordToDouble <$> getWord64BE

-- | Get a number of bytes in bytestring format.
getBytes :: ByteArray bytes => Int -> Unpacking bytes
getBytes n = unpackCheckActRef n $ \ptr ->
    B.create n (\bytesPtr -> B.memCopy bytesPtr ptr n)

-- | Get the remaining bytes.
getRemaining :: ByteArray bytes => Unpacking bytes
getRemaining = unpackGetNbRemaining >>= getBytes

-- | Get a number of bytes until in bytestring format.
--
-- this could be made more efficient
getBytesWhile :: ByteArray bytes => (Word8 -> Bool) -> Unpacking (Maybe bytes)
getBytesWhile predicate = unpackLookahead searchEnd >>= \mn -> maybe (return Nothing) (\n -> Just <$> getBytes n) mn
    where searchEnd :: Ptr Word8 -> Int -> IO (Maybe Int)
          searchEnd ptr sz = loop 0
            where loop :: Int -> IO (Maybe Int)
                  loop i
                     | i >= sz   = return $ Nothing
                     | otherwise = do w <- peek (ptr `plusPtr` i)
                                      if predicate w
                                          then loop (i+1)
                                          else return $ Just i

-- | Get an arbitrary type with the Storable class constraint.
--
-- The Storage method for sizeOf need to be constant size related
-- to the type. It cannot use any fields to define its size.
--
-- The sizeOf method is always going to be called with undefined,
-- so make sure sizeOf doesn't need the value of the type.
getStorable :: Storable a => Unpacking a
getStorable = get_ undefined
    where get_ :: Storable a => a -> Unpacking a
          get_ undefA = unpackCheckAct (sizeOf undefA) (peek . castPtr)

-- | Isolate N bytes from the unpacking, and create an isolated
-- context where only those N bytes are available.
--
-- If the sub unpacker doesn't consume all the bytes available,
-- this function will raises an exception
isolate :: Int -> Unpacking a -> Unpacking a
isolate n subUnpacker = unpackIsolate n subUnpacker

-- | Put a Word8
putWord8 :: Word8 -> Packing ()
putWord8 w = packCheckAct 1 (\ptr -> poke (castPtr ptr) w)

-- | Put a Word8 Hole
putHoleWord8 :: Packing (Hole Word8)
putHoleWord8 = packHole 1 (\ptr w -> poke (castPtr ptr) w)

-- | Put a Word16 in the host endianess.
--
-- It's recommended to use an explicit endianness (LE or BE)
-- when serializing format.
putWord16 :: Word16 -> Packing ()
putWord16 w = packCheckAct 2 (\ptr -> poke (castPtr ptr) w)
{-# INLINE putWord16 #-}

-- | Put a Word16 serialized in little endian.
putWord16LE :: Word16 -> Packing ()
putWord16LE w = putStorable (toLE w)

-- | Put a Word16 serialized in big endian.
putWord16BE :: Word16 -> Packing ()
putWord16BE w = putStorable (toBE w)

putSizedHole_ :: Storable a => Int -> (b -> a) -> Packing (Hole b)
putSizedHole_ size f = packHole size $ \ptr w -> poke (castPtr ptr) (f w)

-- | Put a Word16 Hole
putHoleWord16_ :: Storable a => (Word16 -> a) -> Packing (Hole Word16)
putHoleWord16_ f = putSizedHole_ 2 f

putHoleWord16, putHoleWord16BE, putHoleWord16LE :: Packing (Hole Word16)
-- | Put a Word16 Hole in host endian
putHoleWord16 = putHoleWord16_ id

-- | Put a Word16 Hole in big endian
putHoleWord16BE = putHoleWord16_ toBE

-- | Put a Word16 Hole in little endian
putHoleWord16LE = putHoleWord16_ toBE

-- | Put a Word32 in the host endianess.
--
-- It's recommended to use an explicit endianness (LE or BE)
-- when serializing format.
putWord32 :: Word32 -> Packing ()
putWord32 w = packCheckAct 4 (\ptr -> poke (castPtr ptr) w)
{-# INLINE putWord32 #-}

-- | Put a Word32 serialized in little endian.
putWord32LE :: Word32 -> Packing ()
putWord32LE w = putStorable (toLE w)

-- | Put a Word32 serialized in big endian.
putWord32BE :: Word32 -> Packing ()
putWord32BE w = putStorable (toBE w)

-- | Put a Word32 Hole
putHoleWord32_ :: Storable a => (Word32 -> a) -> Packing (Hole Word32)
putHoleWord32_ = putSizedHole_ 4

putHoleWord32, putHoleWord32BE, putHoleWord32LE :: Packing (Hole Word32)
-- | Put a Word32 Hole in host endian
putHoleWord32 = putHoleWord32_ id

-- | Put a Word32 Hole in big endian
putHoleWord32BE = putHoleWord32_ toBE

-- | Put a Word32 Hole in little endian
putHoleWord32LE = putHoleWord32_ toLE

-- | Put a Word64 in the host endianess.
--
-- It's recommended to use an explicit endianness (LE or BE)
-- when serializing format.
putWord64 :: Word64 -> Packing ()
putWord64 w = packCheckAct 8 (\ptr -> poke (castPtr ptr) w)
{-# INLINE putWord64 #-}

-- | Put a Word64 serialized in little endian.
putWord64LE :: Word64 -> Packing ()
putWord64LE w = putStorable (toLE w)

-- | Put a Word64 serialized in big endian.
putWord64BE :: Word64 -> Packing ()
putWord64BE w = putStorable (toBE w)

-- | Put a Word64 Hole
putHoleWord64_ :: Storable a => (Word64 -> a) -> Packing (Hole Word64)
putHoleWord64_ = putSizedHole_ 8

putHoleWord64, putHoleWord64BE, putHoleWord64LE :: Packing (Hole Word64)
-- | Put a Word64 Hole in host endian
putHoleWord64 = putHoleWord64_ id

-- | Put a Word64 Hole in big endian
putHoleWord64BE = putHoleWord64_ toBE

-- | Put a Word64 Hole in little endian
putHoleWord64LE = putHoleWord64_ toLE

-- | Write a Float in little endian IEEE-754 format
putFloat32LE :: Float -> Packing ()
putFloat32LE = putWord32LE . floatToWord

-- | Write a Float in big endian IEEE-754 format
putFloat32BE :: Float -> Packing ()
putFloat32BE = putWord32BE . floatToWord

-- | Write a Double in little endian IEEE-754 format
putFloat64LE :: Double -> Packing ()
putFloat64LE = putWord64LE . doubleToWord

-- | Write a Double in big endian IEEE-754 format
putFloat64BE :: Double -> Packing ()
putFloat64BE = putWord64BE . doubleToWord

-- | Put a Bytestring.
putBytes :: ByteArrayAccess bytes => bytes -> Packing ()
putBytes bytes =
    packCheckAct len $ \ptr ->
    B.withByteArray bytes $ \ptr2 ->
    B.memCopy ptr ptr2 len
  where
    len = B.length bytes

-- | Put an arbitrary type with the Storable class constraint.
putStorable :: Storable a => a -> Packing ()
putStorable a = packCheckAct (sizeOf a) (\ptr -> poke (castPtr ptr) a)

-- | Unpack a bytestring using a monadic unpack action.
runUnpacking :: ByteArrayAccess bytes => Unpacking a -> bytes -> a
runUnpacking action bytes = unsafeDoIO $ runUnpackingIO bytes action

-- | Similar to 'runUnpacking' but returns an Either type with an exception type in case of failure.
tryUnpacking :: ByteArrayAccess bytes => Unpacking a -> bytes -> Either E.SomeException a
tryUnpacking action bytes = unsafeDoIO $ tryUnpackingIO bytes action

-- | Run packing with a buffer created internally with a monadic action and return the bytestring
runPackingRes :: ByteArray bytes => Int -> Packing a -> (a, bytes)
runPackingRes sz action = unsafeDoIO $ runPackingIO sz action

-- | Run packing with a buffer created internally with a monadic action and return the bytestring
runPacking :: ByteArray bytes => Int -> Packing a -> bytes
runPacking sz action = snd $ runPackingRes sz action
