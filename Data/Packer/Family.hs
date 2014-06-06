module Data.Packer.Family
    ( Unpacking(..)
    , unpackUnsafeAct
    , unpackCheckAct
    , Packing(..)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word

class (Functor m, Applicative m, MonadIO m, Monad m) => Packing m where
    packCheckAct :: Int                 -- ^ number of bytes to reserve
                 -> (Ptr Word8 -> IO a) -- ^ closure to pack bytes
                 -> m a

class (Functor m, Applicative m, MonadIO m, Monad m) => Unpacking m where
    unpackUnsafeActRef :: Int -- ^ number of bytes
                       -> (ForeignPtr Word8 -> Ptr Word8 -> IO a)
                       -> m a
    unpackCheckActRef :: Int
                      -> (ForeignPtr Word8 -> Ptr Word8 -> IO a)
                      -> m a
    unpackIsolate :: Int          -- ^ number of bytes to isolate
                  -> m a
                  -> m a

-- | Similar to unpackUnsafeActRef except that it throw the foreign ptr.
unpackUnsafeAct :: Unpacking m => Int -> (Ptr Word8 -> IO a) -> m a
unpackUnsafeAct n act = unpackUnsafeActRef n (\_ -> act)

-- | Similar to unpackCheckActRef except that it throw the foreign ptr.
unpackCheckAct :: Unpacking m => Int -> (Ptr Word8 -> IO a) -> m a
unpackCheckAct n act = unpackCheckActRef n (\_ -> act)
