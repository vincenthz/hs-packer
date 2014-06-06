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
    , getBytesCopy
    , getBytesWhile
    , getRemaining
    , getRemainingCopy
    , getStorable
    , getFloat32LE
    , getFloat32BE
    , getFloat64LE
    , getFloat64BE
    , isolate
    -- * Packing functions
    , packGetPosition
    , putWord8
    , putWord16
    , putWord16LE
    , putWord16BE
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
import Data.Packer.Family
import Data.Packer.Unsafe
import Data.Packer.IO
import Data.Packer.Strict.Packing
import Data.Packer.Strict.Unpacking
import Data.Packer.Endian
import Data.Packer.IEEE754
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (memcpy, unsafeCreate, toForeignPtr, fromForeignPtr)
import Data.Word
import Foreign.Storable
import System.IO.Unsafe
import qualified Control.Exception as E

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
unpackSkip :: Unpacking u => Int -> u ()
unpackSkip n = unpackCheckAct n (\_ -> return ())

-- | Get a Word8
getWord8 :: Unpacking u => u Word8
getWord8 = unpackCheckAct 1 peek
{-# INLINE getWord8 #-}

-- | Get a Word16 in the host endianess.
--
-- It's recommended to use an explicit endianness (LE or BE)
-- when unserializing format.
getWord16 :: Unpacking u => u Word16
getWord16 = unpackCheckAct 2 (peek . castPtr)
{-# INLINE getWord16 #-}

-- | Get a Word16 serialized in little endian.
getWord16LE :: Unpacking u => u Word16
getWord16LE = unpackCheckAct 2 (peekAnd le16Host . castPtr)
{-# INLINE getWord16LE #-}

-- | Get a Word16 serialized in big endian.
getWord16BE :: Unpacking u => u Word16
getWord16BE = unpackCheckAct 2 (peekAnd be16Host . castPtr)
{-# INLINE getWord16BE #-}

-- | Get a Word32 in the host endianess.
--
-- It's recommended to use an explicit endianness (LE or BE)
-- when unserializing format.
getWord32 :: Unpacking u => u Word32
getWord32 = unpackCheckAct 4 (peek . castPtr)
{-# INLINE getWord32 #-}

-- | Get a Word32 serialized in little endian.
getWord32LE :: Unpacking u => u Word32
getWord32LE = unpackCheckAct 4 (peekAnd le32Host . castPtr)
{-# INLINE getWord32LE #-}

-- | Get a Word32 serialized in big endian.
getWord32BE :: Unpacking u => u Word32
getWord32BE = unpackCheckAct 4 (peekAnd be32Host . castPtr)
{-# INLINE getWord32BE #-}

-- | Get a Word64 in the host endianess.
--
-- It's recommended to use an explicit endianness (LE or BE)
-- when unserializing format.
getWord64 :: Unpacking u => u Word64
getWord64 = unpackCheckAct 8 (peek . castPtr)
{-# INLINE getWord64 #-}

-- | Get a Word64 serialized in little endian.
getWord64LE :: Unpacking u => u Word64
getWord64LE = unpackCheckAct 8 (peekAnd le64Host . castPtr)
{-# INLINE getWord64LE #-}

-- | Get a Word64 serialized in big endian.
getWord64BE :: Unpacking u => u Word64
getWord64BE = unpackCheckAct 8 (peekAnd be64Host . castPtr)
{-# INLINE getWord64BE #-}

-- | Read a Float in little endian IEEE-754 format
getFloat32LE :: Unpacking u => u Float
getFloat32LE = wordToFloat <$> getWord32LE

-- | Read a Float in big endian IEEE-754 format
getFloat32BE :: Unpacking u => u Float
getFloat32BE = wordToFloat <$> getWord32BE

-- | Read a Double in little endian IEEE-754 format
getFloat64LE :: Unpacking u => u Double
getFloat64LE = wordToDouble <$> getWord64LE

-- | Read a Double in big endian IEEE-754 format
getFloat64BE :: Unpacking u => u Double
getFloat64BE = wordToDouble <$> getWord64BE

-- | Get a number of bytes in bytestring format.
--
-- The original block of memory is expected to live for the life of this bytestring,
-- and this is done so by holding the original ForeignPtr.
getBytes :: Unpacking u => Int -> u ByteString
getBytes n = unpackCheckActRef n $ \fptr ptr -> do
                    o <- withForeignPtr fptr $ \origPtr -> return (ptr `minusPtr` origPtr)
                    return $ B.fromForeignPtr fptr o n

-- | Similar to 'getBytes' but copy the bytes to a new bytestring without making reference
-- to the original memory after the copy. this allow the original block of memory to go away.
getBytesCopy :: Unpacking u => Int -> u ByteString
getBytesCopy n = B.copy <$> getBytes n

-- | Get the remaining bytes.
getRemaining :: UnpackingStrict ByteString
getRemaining = unpackGetNbRemaining >>= getBytes

-- | Get the remaining bytes but copy the bytestring and drop any
-- reference from the original function.
getRemainingCopy :: UnpackingStrict ByteString
getRemainingCopy = B.copy <$> getRemaining

-- | Get a number of bytes until in bytestring format.
--
-- this could be made more efficient
getBytesWhile :: (Word8 -> Bool) -> UnpackingStrict (Maybe ByteString)
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
getStorable :: (Unpacking u, Storable a) => u a
getStorable = get_ undefined
    where get_ :: (Unpacking u, Storable a) => a -> u a
          get_ undefA = unpackCheckAct (sizeOf undefA) (peek . castPtr)

-- | Isolate N bytes from the unpacking, and create an isolated
-- context where only those N bytes are available.
--
-- If the sub unpacker doesn't consume all the bytes available,
-- this function will raises an exception
isolate :: Unpacking u => Int -> u a -> u a
isolate n subUnpacker = unpackIsolate n subUnpacker

-- | Put a Word8
putWord8 :: Packing p => Word8 -> p ()
putWord8 w = packCheckAct 1 (\ptr -> poke (castPtr ptr) w)

-- | Put a Word16 in the host endianess.
--
-- It's recommended to use an explicit endianness (LE or BE)
-- when serializing format.
putWord16 :: Packing p => Word16 -> p ()
putWord16 w = packCheckAct 2 (\ptr -> poke (castPtr ptr) w)
{-# INLINE putWord16 #-}

-- | Put a Word16 serialized in little endian.
putWord16LE :: Packing p => Word16 -> p ()
putWord16LE w = putWord16 (le16Host w)

-- | Put a Word16 serialized in big endian.
putWord16BE :: Packing p => Word16 -> p ()
putWord16BE w = putWord16 (be16Host w)

-- | Put a Word32 in the host endianess.
--
-- It's recommended to use an explicit endianness (LE or BE)
-- when serializing format.
putWord32 :: Packing p => Word32 -> p ()
putWord32 w = packCheckAct 4 (\ptr -> poke (castPtr ptr) w)
{-# INLINE putWord32 #-}

-- | Put a Word32 serialized in little endian.
putWord32LE :: Packing p => Word32 -> p ()
putWord32LE w = putWord32 (le32Host w)

-- | Put a Word32 serialized in big endian.
putWord32BE :: Packing p => Word32 -> p ()
putWord32BE w = putWord32 (be32Host w)

-- | Put a Word32 Hole
putHoleWord32_ :: (Word32 -> Word32) -> PackingStrict (Hole Word32)
putHoleWord32_ f = packHole 4 (\ptr w -> poke (castPtr ptr) (f w))

putHoleWord32, putHoleWord32BE, putHoleWord32LE :: PackingStrict (Hole Word32)
-- | Put a Word32 Hole in host endian
putHoleWord32 = putHoleWord32_ id

-- | Put a Word32 Hole in big endian
putHoleWord32BE = putHoleWord32_ be32Host

-- | Put a Word32 Hole in little endian
putHoleWord32LE = putHoleWord32_ le32Host

-- | Put a Word64 in the host endianess.
--
-- It's recommended to use an explicit endianness (LE or BE)
-- when serializing format.
putWord64 :: Packing p => Word64 -> p ()
putWord64 w = packCheckAct 8 (\ptr -> poke (castPtr ptr) w)
{-# INLINE putWord64 #-}

-- | Put a Word64 serialized in little endian.
putWord64LE :: Packing p => Word64 -> p ()
putWord64LE w = putWord64 (le64Host w)

-- | Put a Word64 serialized in big endian.
putWord64BE :: Packing p => Word64 -> p ()
putWord64BE w = putWord64 (be64Host w)

-- | Put a Word64 Hole
putHoleWord64_ :: (Word64 -> Word64) -> PackingStrict (Hole Word64)
putHoleWord64_ f = packHole 8 (\ptr w -> poke (castPtr ptr) (f w))

putHoleWord64, putHoleWord64BE, putHoleWord64LE :: PackingStrict (Hole Word64)
-- | Put a Word64 Hole in host endian
putHoleWord64 = putHoleWord64_ id

-- | Put a Word64 Hole in big endian
putHoleWord64BE = putHoleWord64_ be64Host

-- | Put a Word64 Hole in little endian
putHoleWord64LE = putHoleWord64_ le64Host

-- | Write a Float in little endian IEEE-754 format
putFloat32LE :: Packing p => Float -> p ()
putFloat32LE = putWord32LE . floatToWord

-- | Write a Float in big endian IEEE-754 format
putFloat32BE :: Packing p => Float -> p ()
putFloat32BE = putWord32BE . floatToWord

-- | Write a Double in little endian IEEE-754 format
putFloat64LE :: Packing p => Double -> p ()
putFloat64LE = putWord64LE . doubleToWord

-- | Write a Double in big endian IEEE-754 format
putFloat64BE :: Packing p => Double -> p ()
putFloat64BE = putWord64BE . doubleToWord

-- | Put a Bytestring.
putBytes :: Packing p => ByteString -> p ()
putBytes bs =
    packCheckAct len $ \ptr ->
    withForeignPtr fptr $ \ptr2 ->
    B.memcpy ptr (ptr2 `plusPtr` o) (fromIntegral len)
  where (fptr,o,len) = B.toForeignPtr bs

-- | Put an arbitrary type with the Storable class constraint.
putStorable :: (Storable a, Packing p) => a -> p ()
putStorable a = packCheckAct (sizeOf a) (\ptr -> poke (castPtr ptr) a)

-- | Unpack a bytestring using a monadic unpack action.
runUnpacking :: UnpackingStrict a -> ByteString -> a
runUnpacking action bs = unsafeDoIO $ runUnpackingIO bs action

-- | Similar to 'runUnpacking' but returns an Either type with an exception type in case of failure.
tryUnpacking :: UnpackingStrict a -> ByteString -> Either E.SomeException a
tryUnpacking action bs = unsafeDoIO $ tryUnpackingIO bs action

-- | Run packing with a buffer created internally with a monadic action and return the bytestring
runPackingRes :: Int -> PackingStrict a -> (a, ByteString)
runPackingRes sz action = unsafeDoIO $ runPackingIO sz action

-- | Run packing with a buffer created internally with a monadic action and return the bytestring
runPacking :: Int -> PackingStrict a -> ByteString
runPacking sz action = snd $ runPackingRes sz action
