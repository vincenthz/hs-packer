{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Data.Packer.Strict.Unpacking
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Internal of packer which is a simple state monad that hold
-- a memory pointer and a size of the memory pointed.
--
module Data.Packer.Strict.Unpacking
    ( UnpackingStrict(..)
    , unpackSetPosition
    , unpackGetPosition
    , unpackGetNbRemaining
    , unpackLookahead
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
import Data.Packer.Internal

-- | Unpacking strict monad
newtype UnpackingStrict a = UnpackingStrict { runUnpackingStrict_ :: (ForeignPtr Word8, Memory) -> Memory -> IO (a, Memory) }

instance Monad UnpackingStrict where
    return = returnUnpacking
    (>>=)  = bindUnpacking

instance MonadIO UnpackingStrict where
    liftIO f = UnpackingStrict $ \_ st -> f >>= \a -> return (a,st)

instance Functor UnpackingStrict where
    fmap = fmapUnpacking

instance Applicative UnpackingStrict where
    pure  = returnUnpacking
    (<*>) = apUnpacking

instance Alternative UnpackingStrict where
    empty = error "Data.Packer (Alternative): empty"
    f <|> g = UnpackingStrict $ \cst st ->
        tryRunUnpacking f cst st >>= either (const $ runUnpackingStrict_ g cst st) return

instance Unpacking UnpackingStrict where
    unpackUnsafeActRef = unpackStrictUnsafeActRef
    unpackCheckActRef = unpackStrictCheckActRef
    unpackIsolate = unpackStrictIsolate

tryRunUnpacking :: UnpackingStrict a -> (ForeignPtr Word8, Memory) -> Memory -> IO (Either SomeException (a,Memory))
tryRunUnpacking f cst st = try $ runUnpackingStrict_ f cst st

bindUnpacking :: UnpackingStrict a -> (a -> UnpackingStrict b) -> UnpackingStrict b
bindUnpacking m1 m2 = UnpackingStrict $ \cst st -> do
    (a, st2) <- runUnpackingStrict_ m1 cst st
    runUnpackingStrict_ (m2 a) cst st2
{-# INLINE bindUnpacking #-}

fmapUnpacking :: (a -> b) -> UnpackingStrict a -> UnpackingStrict b
fmapUnpacking f m = UnpackingStrict $ \cst st -> runUnpackingStrict_ m cst st >>= \(a, st2) -> return (f a, st2)
{-# INLINE fmapUnpacking #-}

returnUnpacking :: a -> UnpackingStrict a
returnUnpacking a = UnpackingStrict $ \_ st -> return (a,st)
{-# INLINE [0] returnUnpacking #-}

apUnpacking :: UnpackingStrict (a -> b) -> UnpackingStrict a -> UnpackingStrict b
apUnpacking fm m = fm >>= \p -> m >>= \r2 -> return (p r2)
{-# INLINE [0] apUnpacking #-}

-- | run an action to transform a number of bytes into a 'a'
-- and increment the pointer by number of bytes.
unpackStrictUnsafeActRef :: Int -- ^ number of bytes
                         -> (ForeignPtr Word8 -> Ptr Word8 -> IO a)
                         -> UnpackingStrict a
unpackStrictUnsafeActRef n act = UnpackingStrict $ \(fptr, iniBlock) st@(Memory ptr sz) -> do
    r <- act fptr ptr
    return (r, Memory (ptr `plusPtr` n) (sz - n))

-- | similar 'unpackUnsafeActRef' but does boundary checking.
unpackStrictCheckActRef :: Int
                        -> (ForeignPtr Word8 -> Ptr Word8 -> IO a)
                        -> UnpackingStrict a
unpackStrictCheckActRef n act = UnpackingStrict $ \(fptr, iniBlock@(Memory iniPtr _)) (Memory ptr sz) -> do
    when (sz < n) (throwIO $ OutOfBoundUnpacking (ptr `minusPtr` iniPtr) n)
    r <- act fptr ptr
    return (r, Memory (ptr `plusPtr` n) (sz - n))
{-# INLINE [0] unpackStrictCheckActRef #-}

-- | Isolate a number of bytes to run an unpacking operation.
--
-- If the unpacking doesn't consume all the bytes, an exception is raised.
unpackStrictIsolate :: Int
                    -> UnpackingStrict a
                    -> UnpackingStrict a
unpackStrictIsolate n sub = UnpackingStrict $ \(fptr, iniBlock@(Memory iniPtr _)) (Memory ptr sz) -> do
    when (sz < n) (throwIO $ OutOfBoundUnpacking (ptr `minusPtr` iniPtr) n)
    (r, Memory newPtr subLeft) <- (runUnpackingStrict_ sub) (fptr,iniBlock) (Memory ptr n)
    when (subLeft > 0) $ (throwIO $ IsolationNotFullyConsumed n subLeft)
    return (r, Memory newPtr (sz - n))

-- | Set the new position from the beginning in the memory block.
-- This is useful to skip bytes or when using absolute offsets from a header or some such.
unpackSetPosition :: Int -> UnpackingStrict ()
unpackSetPosition pos = UnpackingStrict $ \(fptr, iniBlock@(Memory iniPtr sz)) _ -> do
    when (pos < 0 || pos >= sz) (throwIO $ OutOfBoundUnpacking pos 0)
    return ((), Memory (iniPtr `plusPtr` pos) (sz-pos))

-- | Get the position in the memory block.
unpackGetPosition :: UnpackingStrict Int
unpackGetPosition = UnpackingStrict $
    \(_, (Memory iniPtr _)) st@(Memory ptr _) -> return (ptr `minusPtr` iniPtr, st)

-- | Return the number of remaining bytes
unpackGetNbRemaining :: UnpackingStrict Int
unpackGetNbRemaining = UnpackingStrict $ \_ st@(Memory _ sz) -> return (sz,st)

-- | Allow to look into the memory.
-- This is inherently unsafe
unpackLookahead :: (Ptr Word8 -> Int -> IO a) -- ^ callback with current position and byte left
                -> UnpackingStrict a
unpackLookahead f = UnpackingStrict $
    \_ st@(Memory ptr sz) -> f ptr sz >>= \a -> return (a, st)
