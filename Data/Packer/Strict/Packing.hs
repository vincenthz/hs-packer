{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Data.Packer.Strict.Packing
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.Packer.Strict.Packing
    ( PackingStrict(..)
    , Hole
    -- * pack methods
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
import Data.Packer.Internal

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
