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
    ( PackingStrict(..)
    , Hole
    , UnpackingStrict(..)
    , Memory(..)
    -- * exceptions
    , OutOfBoundUnpacking(..)
    , OutOfBoundPacking(..)
    , HoleInPacking(..)
    , IsolationNotFullyConsumed(..)
    -- * unpack methods
    --, unpackUnsafeActRef
    --, unpackCheckActRef
    --, unpackUnsafeAct
    --, unpackCheckAct
    --, unpackIsolate
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
import Foreign.ForeignPtr
import Data.Data
import Data.Word
import Control.Exception (Exception, throwIO, try, SomeException)
import Control.Monad.Trans
import Control.Applicative (Alternative(..), Applicative(..), (<$>), (<*>))
import Control.Concurrent.MVar
import Control.Monad (when)
import Data.Packer.Family

-- | Represent a memory block with a ptr as beginning
data Memory = Memory {-# UNPACK #-} !(Ptr Word8)
                     {-# UNPACK #-} !Int

-- | Packing monad
newtype PackingStrict a = PackingStrict { runPackingStrict_ :: (Ptr Word8, MVar Int) -> Memory -> IO (a, Memory) }

instance Monad PackingStrict where
    return = returnPacking
    (>>=)  = bindPacking

instance MonadIO PackingStrict where
    liftIO f = PackingStrict $ \_ st -> f >>= \a -> return (a,st)

instance Functor PackingStrict where
    fmap = fmapPacking

instance Applicative PackingStrict where
    pure  = returnPacking
    (<*>) = apPacking

instance Packing PackingStrict where
    packCheckAct = packStrictCheckAct

bindPacking :: PackingStrict a -> (a -> PackingStrict b) -> PackingStrict b
bindPacking m1 m2 = PackingStrict $ \cst st -> do
    (a, st2) <- runPackingStrict_ m1 cst st
    runPackingStrict_ (m2 a) cst st2
{-# INLINE bindPacking #-}

fmapPacking :: (a -> b) -> PackingStrict a -> PackingStrict b
fmapPacking f m = PackingStrict $ \cst st -> runPackingStrict_ m cst st >>= \(a, st2) -> return (f a, st2)
{-# INLINE fmapPacking #-}

returnPacking :: a -> PackingStrict a
returnPacking a = PackingStrict $ \_ st -> return (a,st)
{-# INLINE [0] returnPacking #-}

apPacking :: PackingStrict (a -> b) -> PackingStrict a -> PackingStrict b
apPacking fm m = fm >>= \p -> m >>= \r2 -> return (p r2)
{-# INLINE [0] apPacking #-}

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

-- | run a pack action on the internal packed buffer.
packStrictCheckAct :: Int -> (Ptr Word8 -> IO a) -> PackingStrict a
packStrictCheckAct n act = PackingStrict $ \_ (Memory ptr sz) -> do
    when (sz < n) (throwIO $ OutOfBoundPacking sz n)
    r <- act ptr
    return (r, Memory (ptr `plusPtr` n) (sz - n))
{-# INLINE [0] packStrictCheckAct #-}

-- | modify holes
modifyHoles :: (Int -> Int) -> PackingStrict ()
modifyHoles f = PackingStrict $ \(_, holesMVar) mem -> modifyMVar_ holesMVar (\v -> return $! f v) >> return ((), mem)

-- | Get the position in the memory block.
packGetPosition :: PackingStrict Int
packGetPosition = PackingStrict $ \(iniPtr, _) mem@(Memory ptr _) -> return (ptr `minusPtr` iniPtr, mem)

-- | A Hole represent something that need to be filled
-- later, for example a CRC, a prefixed size, etc.
--
-- They need to be filled before the end of the package,
-- otherwise an exception will be raised.
newtype Hole a = Hole (a -> IO ())

-- | Put a Hole of a specific size for filling later.
packHole :: Int -> (Ptr Word8 -> a -> IO ()) -> PackingStrict (Hole a)
packHole n f = do
    r <- packCheckAct n (\ptr -> return $ Hole (\w -> f ptr w))
    modifyHoles (1 +)
    return r

-- | Fill a hole with a value
--
-- TODO: user can use one hole many times leading to wrong counting.
fillHole :: Hole a -> a -> PackingStrict ()
fillHole (Hole closure) a = modifyHoles (\i -> i - 1) >> liftIO (closure a)
