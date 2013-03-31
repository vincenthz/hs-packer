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
    , Memory(..)
    , PackSt(..)
    , OutOfBoundUnpacking(..)
    , OutOfBoundPacking(..)
    , HoleInPacking(..)
    , unpackUnsafeActRef
    , unpackCheckActRef
    , unpackUnsafeAct
    , unpackCheckAct
    , unpackLookahead
    , unpackSetPosition
    , unpackGetPosition
    , unpackGetNbRemaining
    , packCheckAct
    , packHole
    , packGetPosition
    , fillHole
    ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Data
import Data.Word
import Control.Exception (Exception, throw, throwIO)
import Control.Monad.State
import Control.Applicative (Applicative(..), (<$>), (<*>))

-- | Represent a memory block with a ptr as beginning
data Memory = Memory {-# UNPACK #-} !(Ptr Word8)
                     {-# UNPACK #-} !Int

-- | Packing state
data PackSt = PackSt (Ptr Word8) !Int !Memory

-- | Packing monad
newtype Packing a = Packing { runPacking_ :: StateT PackSt IO a }
                  deriving (Functor,Applicative,Monad,MonadIO)

-- | Unpacking monad
newtype Unpacking a = Unpacking { runUnpacking_ :: (ForeignPtr Word8, Memory) -> Memory -> IO (a, Memory) }

instance Monad Unpacking where
    return = returnUnpacking
    (>>=)  = bindUnpacking

instance Functor Unpacking where
    fmap = fmapUnpacking

instance Applicative Unpacking where
    pure  = returnUnpacking
    (<*>) = apUnpacking

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

instance Exception OutOfBoundPacking
instance Exception HoleInPacking
instance Exception OutOfBoundUnpacking

unpackUnsafeActRef :: Int -> (ForeignPtr Word8 -> Ptr Word8 -> IO a) -> Unpacking a
unpackUnsafeActRef n act = Unpacking $ \(fptr, iniBlock) st@(Memory ptr sz) -> do
    r <- act fptr ptr
    return (r, Memory (ptr `plusPtr` n) (sz - n))

unpackCheckActRef :: Int -> (ForeignPtr Word8 -> Ptr Word8 -> IO a) -> Unpacking a
unpackCheckActRef n act = Unpacking $ \(fptr, iniBlock@(Memory iniPtr _)) (Memory ptr sz) -> do
    when (sz < n) (throwIO $ OutOfBoundUnpacking (ptr `minusPtr` iniPtr) n)
    r <- act fptr ptr
    return (r, Memory (ptr `plusPtr` n) (sz - n))
{-# INLINE [0] unpackCheckActRef #-}

unpackUnsafeAct :: Int -> (Ptr Word8 -> IO a) -> Unpacking a
unpackUnsafeAct n act = unpackUnsafeActRef n (\_ -> act)

unpackCheckAct :: Int -> (Ptr Word8 -> IO a) -> Unpacking a
unpackCheckAct n act = unpackCheckActRef n (\_ -> act)
{-# INLINE [0] unpackCheckAct #-}

-- | Set the new position from the beginning in the memory block.
-- This is useful to skip bytes or when using absolute offsets from a header or some such.
unpackSetPosition :: Int -> Unpacking ()
unpackSetPosition pos = Unpacking $ \(fptr, iniBlock@(Memory iniPtr sz)) _ -> do
    when (pos < 0 || pos >= sz) (throwIO $ OutOfBoundUnpacking pos 0)
    return ((), Memory (iniPtr `plusPtr` pos) (sz-pos))

-- | Get the position in the memory block.
unpackGetPosition :: Unpacking Int
unpackGetPosition = Unpacking $
    \(_, (Memory iniPtr _)) st@(Memory ptr _) -> return (ptr `minusPtr` iniPtr, st)

unpackGetNbRemaining :: Unpacking Int
unpackGetNbRemaining = Unpacking $
    \_ st@(Memory _ sz) -> return (sz,st)

-- | Allow to look into the memory.
-- This is inherently unsafe
unpackLookahead :: (Ptr Word8 -> Int -> IO a) -- ^ callback with current position and byte left
                -> Unpacking a
unpackLookahead f = Unpacking $
    \_ st@(Memory ptr sz) -> f ptr sz >>= \a -> return (a, st)

withPackMemory :: Int -> (Ptr Word8 -> IO a) -> StateT PackSt IO a
withPackMemory n act = do
    (PackSt iPos holes (Memory ptr sz)) <- get
    when (sz < n) (lift $ throw $ OutOfBoundPacking sz n)
    r <- lift (act ptr)
    put $ PackSt iPos holes (Memory (ptr `plusPtr` n) (sz - n))
    return r

modifyHoles :: (Int -> Int) -> Packing ()
modifyHoles f = Packing $ modify (\(PackSt iPos holes mem) -> PackSt iPos (f holes) mem)

packCheckAct :: Int -> (Ptr Word8 -> IO a) -> Packing a
packCheckAct n act = Packing (withPackMemory n act)

-- | Get the position in the memory block.
packGetPosition :: Packing Int
packGetPosition = Packing $ gets (\(PackSt iniPtr _ (Memory ptr _)) -> ptr `minusPtr` iniPtr)

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
fillHole (Hole closure) a = modifyHoles (\i -> i - 1) >> Packing (lift $ closure a)
