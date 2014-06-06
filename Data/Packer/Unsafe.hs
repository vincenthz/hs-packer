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
import Data.Packer.Strict.Packing
import Data.Packer.Strict.Unpacking
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.State
import Control.Exception
import Control.Concurrent.MVar (takeMVar, newMVar)

-- | Run packing on an arbitrary buffer with a size.
--
-- This is available, for example to run on mmap typed memory, and this is highly unsafe,
-- as the user need to make sure the pointer and size passed to this function are correct.
runPackingAt :: Ptr Word8   -- ^ Pointer to the beginning of the memory
             -> Int         -- ^ Size of the memory
             -> PackingStrict a   -- ^ Packing action
             -> IO (a, Int) -- ^ Number of bytes filled
runPackingAt ptr sz action = do
    mvar <- newMVar 0
    (a, (Memory _ leftSz)) <- (runPackingStrict_ action) (ptr,mvar) (Memory ptr sz)
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
               -> UnpackingStrict a -- ^ Unpacking action
               -> IO a
runUnpackingAt fptr offset sz action =
    withForeignPtr fptr $ \ptr ->
      let m = Memory (ptr `plusPtr` offset) sz
       in fmap fst ((runUnpackingStrict_ action) (fptr,m) m)
