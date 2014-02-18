-- |
-- Module      : Data.Packer.IEEE754
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- IEEE-754 parsing, lifted from the cereal package by Christian Marie <pingu@ponies.io>
--
-- Implementation is described here:
-- <http://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-float/7002812#7002812>
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Packer.IEEE754 (
      wordToDouble
    , wordToFloat
    , doubleToWord
    , floatToWord
) where

import Control.Monad.ST (runST, ST)

import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Word (Word32, Word64)

#if __GLASGOW_HASKELL__ >= 704
import Data.Array.Unsafe (castSTUArray)
#else
import Data.Array.ST (castSTUArray)
#endif

{-# INLINE wordToFloat #-}
wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

{-# INLINE floatToWord #-}
floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

{-# INLINE wordToDouble #-}
wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

{-# INLINE doubleToWord #-}
doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) =>
        a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
