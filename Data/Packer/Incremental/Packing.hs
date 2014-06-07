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
    ( PackingIncremental
    , runPackingIncremental
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

runPackingIncremental :: PackingPusher -> Int -> PackingIncremental () -> IO ()
runPackingIncremental pusher size pi = do
    fptr <- B.mallocByteString size
    withForeignPtr fptr $ \ptr -> do
        (_, PackedBuffer _ nFptr, Memory _ mLeft) <- (runPI_ pi) (PackedBuffer size fptr) pusher (Memory ptr size)
        withForeignPtr nFptr $ \nPtr -> do
            b <- B.createAndTrim size $ \trimedPtr -> B.memcpy trimedPtr nPtr (size - mLeft) >> return (size - mLeft)
            pusher b

type PackingPusher = B.ByteString -> IO ()

data PackedBuffer = PackedBuffer Int (ForeignPtr Word8)

data PackingIncremental a = PackingIncremental
    { runPI_ :: PackedBuffer
             -> PackingPusher
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

                  (npb, nMem) <- withForeignPtr tFptr $ \tptr -> pushAll (PackedBuffer size fPtr) pusher (Memory ptr sz) tptr n

                  -- Create tmp ByteString to flush the content of the
                  return (r, npb, nMem)
              | otherwise = do
                  r <- act ptr
                  return (r, PackedBuffer size fPtr, Memory (ptr `plusPtr` n) (n - sz))
          pushAll :: PackedBuffer -> PackingPusher -> Memory -> Ptr Word8 -> Int -> IO (PackedBuffer, Memory)
          pushAll (PackedBuffer pbSize pbFptr) pusher (Memory ptr mLeft) tmpPtr size =
              case compare size mLeft of
                  LT -> do B.memcpy ptr tmpPtr size
                           return (PackedBuffer pbSize pbFptr, Memory (ptr `plusPtr` size) (mLeft - size))
                  EQ -> do B.memcpy ptr tmpPtr size
                           pusher (B.PS pbFptr 0 pbSize)
                           nFptr <- B.mallocByteString pbSize
                           withForeignPtr nFptr $ \nptr -> return (PackedBuffer pbSize nFptr, Memory nptr pbSize)
                  GT -> do B.memcpy ptr tmpPtr mLeft
                           pusher (B.PS pbFptr 0 pbSize)
                           nFptr <- B.mallocByteString pbSize
                           withForeignPtr nFptr $ \nptr ->
                               pushAll (PackedBuffer pbSize nFptr) pusher (Memory nptr pbSize) tmpPtr (size - mLeft)
