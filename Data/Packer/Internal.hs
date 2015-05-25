{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    ( Packing(..)
    , Hole
    , Unpacking(..)
    , MemView(..)
    -- * exceptions
    , OutOfBoundUnpacking(..)
    , OutOfBoundPacking(..)
    , HoleInPacking(..)
    , IsolationNotFullyConsumed(..)
    -- * unpack methods
    , unpackUnsafeActRef
    , unpackCheckActRef
    , unpackUnsafeAct
    , unpackCheckAct
    , unpackIsolate
    , unpackLookahead
    , unpackSetPosition
    , unpackGetPosition
    , unpackGetNbRemaining
    -- * pack methods
    , packCheckAct
    , packHole
    , packGetPosition
    , fillHole
    ) where

import Foreign.Ptr
import Data.Data
import Data.Word
import Control.Exception (Exception, throwIO, try, SomeException)
import Control.Monad.IO.Class
import Control.Applicative (Alternative(..), Applicative(..), (<$>), (<*>))
import Control.Concurrent.MVar
import Control.Monad (when)

import Data.ByteArray (MemView(..))
import qualified Data.ByteArray as B

memViewPlus :: MemView -> Int -> MemView
memViewPlus (MemView p len) n = MemView (p `plusPtr` n) (len - n)

-- | Packing monad
newtype Packing a = Packing { runPacking_ :: (Ptr Word8, MVar Int) -> MemView -> IO (a, MemView) }

instance Monad Packing where
    return = returnPacking
    (>>=)  = bindPacking

instance MonadIO Packing where
    liftIO f = Packing $ \_ st -> f >>= \a -> return (a,st)

instance Functor Packing where
    fmap = fmapPacking

instance Applicative Packing where
    pure  = returnPacking
    (<*>) = apPacking

bindPacking :: Packing a -> (a -> Packing b) -> Packing b
bindPacking m1 m2 = Packing $ \cst st -> do
    (a, st2) <- runPacking_ m1 cst st
    runPacking_ (m2 a) cst st2
{-# INLINE bindPacking #-}

fmapPacking :: (a -> b) -> Packing a -> Packing b
fmapPacking f m = Packing $ \cst st -> runPacking_ m cst st >>= \(a, st2) -> return (f a, st2)
{-# INLINE fmapPacking #-}

returnPacking :: a -> Packing a
returnPacking a = Packing $ \_ st -> return (a,st)
{-# INLINE [0] returnPacking #-}

apPacking :: Packing (a -> b) -> Packing a -> Packing b
apPacking fm m = fm >>= \p -> m >>= \r2 -> return (p r2)
{-# INLINE [0] apPacking #-}

-- | Unpacking monad
newtype Unpacking a = Unpacking { runUnpacking_ :: MemView -> MemView -> IO (a, MemView) }

instance Monad Unpacking where
    return = returnUnpacking
    (>>=)  = bindUnpacking

instance MonadIO Unpacking where
    liftIO f = Unpacking $ \_ st -> f >>= \a -> return (a,st)

instance Functor Unpacking where
    fmap = fmapUnpacking

instance Applicative Unpacking where
    pure  = returnUnpacking
    (<*>) = apUnpacking

instance Alternative Unpacking where
    empty = error "Data.Packer (Alternative): empty"
    f <|> g = Unpacking $ \cst st ->
        tryRunUnpacking f cst st >>= either (const $ runUnpacking_ g cst st) return

tryRunUnpacking :: Unpacking a -> MemView -> MemView -> IO (Either SomeException (a,MemView))
tryRunUnpacking f cst st = try $ runUnpacking_ f cst st

bindUnpacking :: Unpacking a -> (a -> Unpacking b) -> Unpacking b
bindUnpacking m1 m2 = Unpacking $ \cst st -> do
    (a, st2) <- runUnpacking_ m1 cst st
    runUnpacking_ (m2 a) cst st2
{-# INLINE bindUnpacking #-}

fmapUnpacking :: (a -> b) -> Unpacking a -> Unpacking b
fmapUnpacking f m = Unpacking $ \cst st -> runUnpacking_ m cst st >>= \(a, st2) -> return (f a, st2)
{-# INLINE fmapUnpacking #-}

returnUnpacking :: a -> Unpacking a
returnUnpacking a = Unpacking $ \_ st -> return (a,st)
{-# INLINE [0] returnUnpacking #-}

apUnpacking :: Unpacking (a -> b) -> Unpacking a -> Unpacking b
apUnpacking fm m = fm >>= \p -> m >>= \r2 -> return (p r2)
{-# INLINE [0] apUnpacking #-}

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

-- | run an action to transform a number of bytes into a 'a'
-- and increment the pointer by number of bytes.
unpackUnsafeActRef :: Int -- ^ number of bytes
                   -> (Ptr Word8 -> IO a)
                   -> Unpacking a
unpackUnsafeActRef n act = Unpacking $ \_ memView@(MemView ptr sz) -> do
    r <- act ptr
    return (r, memViewPlus memView n)

-- | similar 'unpackUnsafeActRef' but does boundary checking.
unpackCheckActRef :: Int
                  -> (Ptr Word8 -> IO a)
                  -> Unpacking a
unpackCheckActRef n act = Unpacking $ \(MemView iniPtr _) memView@(MemView ptr sz) -> do
    when (sz < n) (throwIO $ OutOfBoundUnpacking (ptr `minusPtr` iniPtr) n)
    r <- act ptr
    return (r, memViewPlus memView n)
{-# INLINE [0] unpackCheckActRef #-}

-- | Isolate a number of bytes to run an unpacking operation.
--
-- If the unpacking doesn't consume all the bytes, an exception is raised.
unpackIsolate :: Int
              -> Unpacking a
              -> Unpacking a
unpackIsolate n sub = Unpacking $ \iniBlock@(MemView iniPtr _) memView@(MemView ptr sz) -> do
    when (sz < n) (throwIO $ OutOfBoundUnpacking (ptr `minusPtr` iniPtr) n)
    (r, MemView newPtr subLeft) <- (runUnpacking_ sub) iniBlock (MemView ptr n)
    when (subLeft > 0) $ (throwIO $ IsolationNotFullyConsumed n subLeft)
    return (r, MemView newPtr (sz - n))

-- | Similar to unpackUnsafeActRef except that it throw the foreign ptr.
unpackUnsafeAct :: Int -> (Ptr Word8 -> IO a) -> Unpacking a
unpackUnsafeAct = unpackUnsafeActRef

-- | Similar to unpackCheckActRef except that it throw the foreign ptr.
unpackCheckAct :: Int -> (Ptr Word8 -> IO a) -> Unpacking a
unpackCheckAct = unpackCheckActRef
{-# INLINE [0] unpackCheckAct #-}

-- | Set the new position from the beginning in the memory block.
-- This is useful to skip bytes or when using absolute offsets from a header or some such.
unpackSetPosition :: Int -> Unpacking ()
unpackSetPosition pos = Unpacking $ \(iniBlock@(MemView _ sz)) _ -> do
    when (pos < 0 || pos > sz) (throwIO $ OutOfBoundUnpacking pos 0)
    return ((), memViewPlus iniBlock pos)

-- | Get the position in the memory block.
unpackGetPosition :: Unpacking Int
unpackGetPosition = Unpacking $
    \(MemView iniPtr _) st@(MemView ptr _) -> return (ptr `minusPtr` iniPtr, st)

-- | Return the number of remaining bytes
unpackGetNbRemaining :: Unpacking Int
unpackGetNbRemaining = Unpacking $ \_ st@(MemView _ sz) -> return (sz,st)

-- | Allow to look into the memory.
-- This is inherently unsafe
unpackLookahead :: (Ptr Word8 -> Int -> IO a) -- ^ callback with current position and byte left
                -> Unpacking a
unpackLookahead f = Unpacking $
    \_ st@(MemView ptr sz) -> f ptr sz >>= \a -> return (a, st)

-- | run a pack action on the internal packed buffer.
packCheckAct :: Int -> (Ptr Word8 -> IO a) -> Packing a
packCheckAct n act = Packing $ \_ (MemView ptr sz) -> do
    when (sz < n) (throwIO $ OutOfBoundPacking sz n)
    r <- act ptr
    return (r, MemView (ptr `plusPtr` n) (sz - n))
{-# INLINE [0] packCheckAct #-}

-- | modify holes
modifyHoles :: (Int -> Int) -> Packing ()
modifyHoles f = Packing $ \(_, holesMVar) mem -> modifyMVar_ holesMVar (\v -> return $! f v) >> return ((), mem)

-- | Get the position in the memory block.
packGetPosition :: Packing Int
packGetPosition = Packing $ \(iniPtr, _) mem@(MemView ptr _) -> return (ptr `minusPtr` iniPtr, mem)

-- | A Hole represent something that need to be filled
-- later, for example a CRC, a prefixed size, etc.
--
-- They need to be filled before the end of the package,
-- otherwise an exception will be raised.
newtype Hole a = Hole (a -> IO ())

-- | Put a Hole of a specific size for filling later.
packHole :: Int -> (Ptr Word8 -> a -> IO ()) -> Packing (Hole a)
packHole n f = do
    r <- packCheckAct n (\ptr -> return $ Hole (\w -> f ptr w))
    modifyHoles (1 +)
    return r

-- | Fill a hole with a value
--
-- TODO: user can use one hole many times leading to wrong counting.
fillHole :: Hole a -> a -> Packing ()
fillHole (Hole closure) a = modifyHoles (\i -> i - 1) >> liftIO (closure a)
