-- |
-- Module      : Data.Packer.Incremental.Packing
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Incremental Packing module
--
{-# LANGUAGE CPP #-}
module Data.Packer.Incremental.Packing
    (
    ) where

import Data.Packer.Family
import Data.Packer.Internal
import Control.Applicative
import Control.Monad.Trans
import Control.Concurrent.MVar
import Control.Monad (liftM2)

import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Data
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

type Pusher = B.ByteString -> IO ()

data PackedBuffer = PackedBuffer Int (ForeignPtr Word8)

data PackingIncremental a = PackingIncremental
    { runPI_ :: PackedBuffer
             -> Pusher
             -> Memory
             -> IO (a, PackedBuffer, Memory) }

instance Functor PackingIncremental where
    fmap = fmapPackingIncremental

instance Applicative PackingIncremental where
    pure  = returnPackingIncremental
    (<*>) = apPackingIncremental

instance Monad PackingIncremental where
    return = returnPackingIncremental
    (>>=)  = bindPackingIncremental

instance MonadIO PackingIncremental where
    liftIO f = PackingIncremental $ \pb _ st -> f >>= \a -> return (a, pb, st)

fmapPackingIncremental :: (a -> b) -> PackingIncremental a -> PackingIncremental b
fmapPackingIncremental f piA = PackingIncremental $ \pb p st ->
    (runPI_ piA) pb p st >>= \(a, npb, nSt) -> return (f a, npb, nSt)
{-# INLINE [0] fmapPackingIncremental #-}

returnPackingIncremental :: a -> PackingIncremental a
returnPackingIncremental a = PackingIncremental $ \pb _ st -> return (a, pb, st)
{-# INLINE [0] returnPackingIncremental #-}

bindPackingIncremental :: PackingIncremental a -> (a -> PackingIncremental b) -> PackingIncremental b
bindPackingIncremental m1 m2 = PackingIncremental $ \pb p st -> do
    (a, pb2, st2) <- (runPI_ m1) pb p st
    (runPI_ (m2 a)) pb2 p st2
{-# INLINE bindPackingIncremental #-}

apPackingIncremental :: PackingIncremental (a -> b) -> PackingIncremental a -> PackingIncremental b
apPackingIncremental fm m = fm >>= \p -> m >>= \r2 -> return (p r2)
{-# INLINE [0] apPackingIncremental #-}

instance Packing PackingIncremental where
    packCheckAct = packIncrementalCheckAct

packIncrementalCheckAct :: Int -> (Ptr Word8 -> IO a) -> PackingIncremental a
packIncrementalCheckAct n act = PackingIncremental exPackIncr
    where exPackIncr (PackedBuffer size fPtr) pusher (Memory ptr sz)
              | sz < n = do
                  tFptr <- B.mallocByteString n
                  r <- withForeignPtr tFptr act

                  tmpPtr <- withForeignPtr tFptr $ \tptr -> B.memcpy ptr tptr sz >> return (tptr `plusPtr` sz)
                  pusher (B.PS fPtr 0 size)
                  -- TODO: loop until n > size

                  -- Create a new Foreign Ptr
                  nFptr <- B.mallocByteString size
                  nptr <- withForeignPtr nFptr $ \nptr -> B.memcpy nptr tmpPtr (size - sz) >> return (nptr `plusPtr` sz)
                  -- Create tmp ByteString to flush the content of the
                  return (r, PackedBuffer size nFptr, Memory nptr (size - sz))
              | otherwise = do
                  r <- act ptr
                  return (r, PackedBuffer size fPtr, Memory (ptr `plusPtr` n) (n - sz))
