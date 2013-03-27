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
    , UnpackSt(..)
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
import Control.Exception (Exception, throw)
import Control.Monad.State
import Control.Applicative (Applicative, (<$>), (<*>))

-- | Represent a memory block with a ptr as beginning
data Memory = Memory {-# UNPACK #-} !(Ptr Word8)
                     {-# UNPACK #-} !Int

-- | Unpacking state
data UnpackSt = UnpackSt !(ForeignPtr Word8) !Memory {-# UNPACK #-} !Memory

-- | Packing state
data PackSt = PackSt (Ptr Word8) !Int !Memory

-- | Packing monad
newtype Packing a = Packing { runPacking_ :: StateT PackSt IO a }
                  deriving (Functor,Applicative,Monad,MonadIO)

-- | Unpacking monad
newtype Unpacking a = Unpacking { runUnpacking_ :: StateT UnpackSt IO a }
                 deriving (Functor,Applicative,Monad,MonadIO)

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

-- TODO not firing probably because of earlier inlining ?
{-# RULES
"check/check merged" forall n m f g.
  (unpackCheckAct n f) <*> (unpackCheckAct m g) = unpackCheckAct (n+m) (\ptr -> f ptr <*> g (ptr `plusPtr` n))
"checkRef/checkRef merged" forall n m f g.
  (unpackCheckActRef n f) <*> (unpackCheckActRef m g) = unpackCheckActRef (n+m) (\r ptr -> f r ptr <*> g r (ptr `plusPtr` n))
  #-}

unpackUnsafeActRef :: Int -> (ForeignPtr Word8 -> Ptr Word8 -> IO a) -> Unpacking a
unpackUnsafeActRef n act = Unpacking $ do
    (UnpackSt fptr iniBlock (Memory ptr sz)) <- get
    r <- lift (act fptr ptr)
    put (UnpackSt fptr iniBlock (Memory (ptr `plusPtr` n) (sz - n)))
    return r

unpackCheckActRef :: Int -> (ForeignPtr Word8 -> Ptr Word8 -> IO a) -> Unpacking a
unpackCheckActRef n act = Unpacking $ do
    (UnpackSt fptr iniBlock@(Memory iniPtr _) (Memory ptr sz)) <- get
    when (sz < n) (lift $ throw $ OutOfBoundUnpacking (ptr `minusPtr` iniPtr) n)
    r <- lift (act fptr ptr)
    put (UnpackSt fptr iniBlock (Memory (ptr `plusPtr` n) (sz - n)))
    return r

unpackUnsafeAct :: Int -> (Ptr Word8 -> IO a) -> Unpacking a
unpackUnsafeAct n act = unpackUnsafeActRef n (\_ -> act)

unpackCheckAct :: Int -> (Ptr Word8 -> IO a) -> Unpacking a
unpackCheckAct n act = unpackCheckActRef n (\_ -> act)

-- | Set the new position from the beginning in the memory block.
-- This is useful to skip bytes or when using absolute offsets from a header or some such.
unpackSetPosition :: Int -> Unpacking ()
unpackSetPosition pos = Unpacking $ do
    (UnpackSt fptr iniBlock@(Memory iniPtr sz) _) <- get
    when (pos < 0 || pos >= sz) (lift $ throw $ OutOfBoundUnpacking pos 0)
    put (UnpackSt fptr iniBlock (Memory (iniPtr `plusPtr` pos) (sz-pos)))

-- | Get the position in the memory block.
unpackGetPosition :: Unpacking Int
unpackGetPosition = Unpacking $ gets (\(UnpackSt _ (Memory iniPtr _) (Memory ptr _)) -> ptr `minusPtr` iniPtr)

unpackGetNbRemaining :: Unpacking Int
unpackGetNbRemaining = Unpacking $ gets (\(UnpackSt _ _ (Memory _ sz)) -> sz)

-- | Allow to look into the memory.
-- This is inherently unsafe
unpackLookahead :: (Ptr Word8 -> Int -> IO a) -- ^ callback with current position and byte left
                -> Unpacking a
unpackLookahead f = Unpacking $ do
    (UnpackSt _ _ (Memory ptr sz)) <- get
    lift $ f ptr sz

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
