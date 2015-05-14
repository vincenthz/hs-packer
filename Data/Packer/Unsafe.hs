-- |
-- Module      : Data.Packer.Unsafe
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Access to lower primitive that allow to use Packing and Unpacking,
-- on mmap type of memory. Potentially unsafe, as it can't check if
-- any of value passed are valid.
--
module Data.Packer.Unsafe
    ( runPackingAt
    , runUnpackingAt
    ) where

import Data.Word
import Data.Packer.Internal
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad
import Control.Exception
import Control.Concurrent.MVar (takeMVar, newMVar)

-- | Run packing on an arbitrary buffer with a size.
--
-- This is available, for example to run on mmap typed memory, and this is highly unsafe,
-- as the user need to make sure the pointer and size passed to this function are correct.
runPackingAt :: Ptr Word8   -- ^ Pointer to the beginning of the memory
             -> Int         -- ^ Size of the memory
             -> Packing a   -- ^ Packing action
             -> IO (a, Int) -- ^ Number of bytes filled
runPackingAt ptr sz action = do
    mvar <- newMVar 0
    (a, (Memory _ leftSz)) <- (runPacking_ action) (ptr,mvar) (Memory ptr sz)
    --(PackSt _ holes (Memory _ leftSz)) <- execStateT (runPacking_ action) (PackSt ptr 0 $ Memory ptr sz)
    holes <- takeMVar mvar
    when (holes > 0) (throwIO $ HoleInPacking holes)
    return (a, sz - leftSz)

-- | Run unpacking on an arbitrary buffer with a size.
--
-- This is available, for example to run on mmap typed memory, and this is highly unsafe,
-- as the user need to make sure the pointer and size passed to this function are correct.
runUnpackingAt :: ForeignPtr Word8 -- ^ The initial block of memory
               -> Int              -- ^ Starting offset in the block of memory
               -> Int              -- ^ Size
               -> Unpacking a      -- ^ Unpacking action
               -> IO a
runUnpackingAt fptr offset sz action =
    withForeignPtr fptr $ \ptr ->
      let m = Memory (ptr `plusPtr` offset) sz
       in fmap fst ((runUnpacking_ action) (fptr,m) m)
