-- |
-- Module      : Data.Packer.Endian
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Simple module to handle endianness swapping,
-- but GHC should provide (some day) primitives to call
-- into a cpu optimised version (e.g. bswap for x86)
--
{-# LANGUAGE CPP #-}
module Data.Packer.Endian
    ( le16Host
    , le32Host
    , le64Host
    , be16Host
    , be32Host
    , be64Host
    ) where

import Data.Bits
import Data.Word

#if MIN_VERSION_base(4,7,0)
-- | swap endianness on a Word16
swap16 :: Word16 -> Word16
swap16 = byteSwap16

-- | Transform a 32 bit value bytes from a.b.c.d to d.c.b.a
swap32 :: Word32 -> Word32
swap32 = byteSwap32

-- | Transform a 64 bit value bytes from a.b.c.d.e.f.g.h to h.g.f.e.d.c.b.a
swap64 :: Word64 -> Word64
swap64 = byteSwap64

#else
#if BITS_IS_OLD
shr :: Bits a => a -> Int -> a
shr = shiftR
shl :: Bits a => a -> Int -> a
shl = shiftL
#else
shr :: Bits a => a -> Int -> a
shr = unsafeShiftR
shl :: Bits a => a -> Int -> a
shl = unsafeShiftL
#endif

-- | swap endianness on a Word64
-- 56 48 40 32 24 16  8  0
--  a  b  c  d  e  f  g  h 
--  h  g  f  e  d  c  b  a
swap64 :: Word64 -> Word64
swap64 w =
        (w `shr` 56)                  .|. (w `shl` 56)
    .|. ((w `shr` 40) .&. 0xff00)     .|. ((w .&. 0xff00) `shl` 40)
    .|. ((w `shr` 24) .&. 0xff0000)   .|. ((w .&. 0xff0000) `shl` 24)
    .|. ((w `shr` 8)  .&. 0xff000000) .|. ((w .&. 0xff000000) `shl` 8)

-- | swap endianness on a Word32
swap32 :: Word32 -> Word32
swap32 w =
        (w `shr` 24)             .|. (w `shl` 24)
    .|. ((w `shr` 8) .&. 0xff00) .|. ((w .&. 0xff00) `shl` 8)

-- | swap endianness on a Word16
swap16 :: Word16 -> Word16
swap16 w = (w `shr` 8) .|. (w `shl` 8)
#endif

#ifdef CPU_BIG_ENDIAN
-- | 16 bit big endian to host endian
{-# INLINE be16Host #-}
be16Host :: Word16 -> Word16
be16Host = id
-- | 32 bit big endian to host endian
{-# INLINE be32Host #-}
be32Host :: Word32 -> Word32
be32Host = id
-- | 64 bit big endian to host endian
{-# INLINE be64Host #-}
be64Host :: Word64 -> Word64
be64Host = id
-- | 16 bit little endian to host endian
le16Host :: Word16 -> Word16
le16Host w = swap16 w
-- | 32 bit little endian to host endian
le32Host :: Word32 -> Word32
le32Host w = swap32 w
-- | 64 bit little endian to host endian
le64Host :: Word64 -> Word64
le64Host w = swap64 w
#else
-- | 16 bit little endian to host endian
{-# INLINE le16Host #-}
le16Host :: Word16 -> Word16
le16Host = id
-- | 32 bit little endian to host endian
{-# INLINE le32Host #-}
le32Host :: Word32 -> Word32
le32Host = id
-- | 64 bit little endian to host endian
{-# INLINE le64Host #-}
le64Host :: Word64 -> Word64
le64Host = id
-- | 16 bit big endian to host endian
be16Host :: Word16 -> Word16
be16Host w = swap16 w
-- | 32 bit big endian to host endian
be32Host :: Word32 -> Word32
be32Host w = swap32 w
-- | 64 bit big endian to host endian
be64Host :: Word64 -> Word64
be64Host w = swap64 w
#endif
