-- |
-- Module      : Data.Packer.Incremental.Unpacking
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.Packer.Incremental.Unpacking
    ( UnpackingIncr
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
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

type UnpackingPopper = IO B.ByteString

-- | Unpacking strict monad
newtype UnpackingIncr a = UnpackingIncr
    { runUnpackingIncr_ :: UnpackingPopper
                        -> ForeignPtr Word8 -- ^ current buffer fptr
                        -> Memory           -- ^ current state in the memory
                        -> IO (a, ForeignPtr Word8, Memory) }

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

{-
instance Alternative UnpackingStrict where
    empty = error "Data.Packer (Alternative): empty"
    f <|> g = UnpackingStrict $ \cst st ->
        tryRunUnpacking f cst st >>= either (const $ runUnpackingStrict_ g cst st) return
-}

instance Unpacking UnpackingIncr where
    unpackUnsafeActRef = unpackIncrUnsafeActRef
    unpackCheckActRef = unpackIncrCheckActRef

    unpackIsolate = undefined

{-
tryRunUnpacking :: UnpackingStrict a -> (ForeignPtr Word8, Memory) -> Memory -> IO (Either SomeException (a,Memory))
tryRunUnpacking f cst st = try $ runUnpackingStrict_ f cst st
-}

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
                    -- Copy what we need from next buffer in the temp buffer
                    -- expect next buffer is enough. obviously not true.

                    -- ------------ should be a loop
                    (B.PS nfPtr nOfs nLen) <- popper
                    next <- withForeignPtr nfPtr $ \nPtr -> do
                        B.memcpy (tPtr `plusPtr` sz) (nPtr `plusPtr` nOfs) (n - sz)
                        return $ Memory (nPtr `plusPtr` (nOfs + n - sz)) (nLen - sz)
                    -- ------------------------

                    -- execute act on the temporary buffer
                    r <- act tfptr tPtr

                    -- then return the value, the new buffer fptr, and the actual memory object associated
                    return (r, nfPtr, next)
            | otherwise = do
                r <- act fptr ptr
                return (r, fptr, Memory (ptr `plusPtr` n) (sz - n))
{-# INLINE [0] unpackIncrCheckActRef #-}

