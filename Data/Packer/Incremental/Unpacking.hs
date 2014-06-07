-- |
-- Module      : Data.Packer.Incremental.Unpacking
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.Packer.Incremental.Unpacking
    ( UnpackingIncr
    , runUnpackingIncr
    , runUnpackingLazy
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L (chunk)
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
import Data.Packer.Internal
import System.IO.Unsafe

type UnpackingPopper = IO B.ByteString

-- | Unpacking strict monad
newtype UnpackingIncr a = UnpackingIncr
    { runUnpackingIncr_ :: UnpackingPopper
                        -> ForeignPtr Word8 -- ^ current buffer fptr
                        -> Memory           -- ^ current state in the memory
                        -> IO (a, ForeignPtr Word8, Memory) }

runUnpackingIncr :: UnpackingPopper
                 -> UnpackingIncr a
                 -> IO a
runUnpackingIncr popper unpacker = do
    (B.PS fptr ofs len) <- popper
    withForeignPtr fptr $ \ptr -> do
        (r, _, _) <- runUnpackingIncr_ unpacker popper fptr (Memory (ptr `plusPtr` ofs) len)
        return r

runUnpackingLazy :: L.ByteString
                 -> UnpackingIncr a
                 -> a
runUnpackingLazy lbs f = unsafeDoIO (newMVar (L.toChunks lbs) >>= \var -> runUnpackingIncr (popper var) f)
  where popper :: MVar [B.ByteString] -> IO B.ByteString
        popper mvar = modifyMVar mvar pop
        pop (x:xs) = return (xs, x)
        pop []     = return ([], B.empty)

instance Monad UnpackingIncr where
    return = returnUnpacking
    (>>=)  = bindUnpacking

instance MonadIO UnpackingIncr where
    liftIO f = UnpackingIncr $ \_ fptr st -> f >>= \a -> return (a,fptr,st)

instance Functor UnpackingIncr where
    fmap = fmapUnpacking

instance Applicative UnpackingIncr where
    pure  = returnUnpacking
    (<*>) = apUnpacking

instance Unpacking UnpackingIncr where
    unpackUnsafeActRef = unpackIncrUnsafeActRef
    unpackCheckActRef = unpackIncrCheckActRef

    unpackIsolate = undefined

bindUnpacking :: UnpackingIncr a -> (a -> UnpackingIncr b) -> UnpackingIncr b
bindUnpacking m1 m2 = UnpackingIncr $ \popper fptr st -> do
    (a, fptr2, st2) <- runUnpackingIncr_ m1 popper fptr st
    runUnpackingIncr_ (m2 a) popper fptr2 st2
{-# INLINE bindUnpacking #-}

fmapUnpacking :: (a -> b) -> UnpackingIncr a -> UnpackingIncr b
fmapUnpacking f m = UnpackingIncr $ \popper fptr st ->
    runUnpackingIncr_ m popper fptr st >>= \(a, fptr2, st2) -> return (f a, fptr2, st2)
{-# INLINE fmapUnpacking #-}

returnUnpacking :: a -> UnpackingIncr a
returnUnpacking a = UnpackingIncr $ \_ fptr st -> return (a,fptr,st)
{-# INLINE [0] returnUnpacking #-}

apUnpacking :: UnpackingIncr (a -> b) -> UnpackingIncr a -> UnpackingIncr b
apUnpacking fm m = fm >>= \p -> m >>= \r2 -> return (p r2)
{-# INLINE [0] apUnpacking #-}

-- | run an action to transform a number of bytes into a 'a'
-- and increment the pointer by number of bytes.
unpackIncrUnsafeActRef :: Int -- ^ number of bytes
                       -> (ForeignPtr Word8 -> Ptr Word8 -> IO a)
                       -> UnpackingIncr a
unpackIncrUnsafeActRef n act = UnpackingIncr $ \_ fptr st@(Memory ptr sz) -> do
    r <- act fptr ptr
    return (r, fptr, Memory (ptr `plusPtr` n) (sz - n))

-- | similar 'unpackUnsafeActRef' but does boundary checking.
unpackIncrCheckActRef :: Int
                      -> (ForeignPtr Word8 -> Ptr Word8 -> IO a)
                      -> UnpackingIncr a
unpackIncrCheckActRef n act = UnpackingIncr exUnpack
  where exUnpack popper fptr (Memory ptr sz)
            | sz < n    = do
                -- copy the remaining bytes from the Memory object to a new bigger temporary buffer
                (B.PS tfptr _ _) <- B.create n $ \ptrDst -> B.memcpy ptrDst ptr sz
                withForeignPtr tfptr $ \tPtr -> do
                    (nfPtr, next) <- fillAll popper $ Memory (tPtr `plusPtr` sz) (n - sz)
                    r <- act tfptr tPtr

                    -- then return the value, the new buffer fptr, and the actual memory object associated
                    return (r, nfPtr, next)
            | otherwise = do
                r <- act fptr ptr
                return (r, fptr, Memory (ptr `plusPtr` n) (sz - n))
        -- Fill the temporary buffer with the pop'ed data, and
        -- return the remaining data
        fillAll :: UnpackingPopper -> Memory -> IO (ForeignPtr Word8, Memory)
        fillAll popper (Memory tPtr left) = do
            -- FIXME handle the case where the popper return empty data
            (B.PS nfPtr nOfs nLen) <- popper
            case compare nLen left of
                EQ -> do
                    withForeignPtr nfPtr $ \nPtr ->
                        B.memcpy tPtr (nPtr `plusPtr` nOfs) left
                    -- consumed all the new buffer, so pop another one.
                    (B.PS nfPtr2 nOfs2 nLen2) <- popper
                    ptr2 <- withForeignPtr nfPtr2 $ \nPtr2 -> return (nPtr2 `plusPtr` nOfs2)
                    return (nfPtr2, Memory ptr2 nLen2)
                GT -> do
                    -- copy part of the new buffer, and return the consumed buffer (but not empty)
                    mem <- withForeignPtr nfPtr $ \nPtr -> do
                        B.memcpy tPtr (nPtr `plusPtr` nOfs) left
                        return (Memory (nPtr `plusPtr` (nOfs + left)) (nLen - left))
                    return (nfPtr, mem)
                LT -> do
                    -- don't have enough yet to fill the temporary buffer, so consume and recurse.
                    withForeignPtr nfPtr $ \nPtr -> B.memcpy tPtr (nPtr `plusPtr` nOfs) nLen
                    fillAll popper $ Memory (tPtr `plusPtr` nLen) (left - nLen)
{-# INLINE [0] unpackIncrCheckActRef #-}

