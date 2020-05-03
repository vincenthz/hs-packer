-- |
-- Module      : Data.Packer.IEEE754
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- fully rewritten to use primops.
--
-- original implementation based on IEEE-754 parsing, lifted from the cereal package by Christian Marie <pingu@ponies.io>
-- Implementation is described here:
-- <http://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-float/7002812#7002812>
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module Data.Packer.IEEE754 (
      wordToDouble
    , wordToFloat
    , doubleToWord
    , floatToWord
) where

#if ! MIN_VERSION_base (4,11,0)
import Data.Word (Word32, Word64)
#endif

import GHC.Prim
import GHC.Float
import GHC.Word
import GHC.ST

wordToFloat :: Word32 -> Float
wordToFloat (W32# x) = runST $ ST $ \s1 ->
    case newByteArray# 4# s1 of
        (# s2, mbarr #) ->
            let !s3 = writeWord32Array# mbarr 0# x s2
             in case readFloatArray# mbarr 0# s3 of
                    (# s4, f #) -> (# s4, F# f #)
{-# INLINE wordToFloat #-}

floatToWord :: Float -> Word32
floatToWord (F# x) = runST $ ST $ \s1 ->
    case newByteArray# 4# s1 of
        (# s2, mbarr #) ->
            let !s3 = writeFloatArray# mbarr 0# x s2
             in case readWord32Array# mbarr 0# s3 of
                    (# s4, w #) -> (# s4, W32# w #)
{-# INLINE floatToWord #-}

wordToDouble :: Word64 -> Double
wordToDouble (W64# x) = runST $ ST $ \s1 ->
    case newByteArray# 8# s1 of
        (# s2, mbarr #) ->
            let !s3 = writeWord64Array# mbarr 0# x s2
             in case readDoubleArray# mbarr 0# s3 of
                    (# s4, f #) -> (# s4, D# f #)
{-# INLINE wordToDouble #-}

doubleToWord :: Double -> Word64
doubleToWord (D# x) = runST $ ST $ \s1 ->
    case newByteArray# 8# s1 of
        (# s2, mbarr #) ->
            let !s3 = writeDoubleArray# mbarr 0# x s2
             in case readWord64Array# mbarr 0# s3 of
                    (# s4, w #) -> (# s4, W64# w #)
{-# INLINE doubleToWord #-}
