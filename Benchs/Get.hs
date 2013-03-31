{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Bits
import Data.Word
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString.Lazy.Builder as Builder

import qualified Data.Serialize.Put as Cereal
import qualified Data.Serialize as Cereal

import qualified Data.Binary.Put as Binary
import qualified Data.Binary as Binary

import qualified Data.Packer as Packer

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Impl
import Data

--benchAll :: (NFData a, ImplPacker a) => 
benchAll a = [ benchBclassGet a x, benchPackerGet a x, benchCerealGet a x, benchBinaryGet a x ]
    where !x = implPutPacker a

main = defaultMain
    [ bgroup "word x 4" $ benchAll wordseq
    , bgroup "pascalstr"
        [ bgroup "8" $ benchAll p8
        , bgroup "32" $ benchAll p32
        , bgroup "64" $ benchAll p64
        , bgroup "256" $ benchAll p256
        , bgroup "1024" $ benchAll p1024
        -- , bgroup "words4 x 1000" $ benchAll wordseqs1000
        ]
    , bgroup "big"
        [ bgroup "words4 x 50" $ benchAll wordseqs50
        , bgroup "words4 x 1000" $ benchAll wordseqs1000
        ]
    ]
    where !b8    = B.replicate 8 0xf7
          !b32   = B.replicate 32 0xf7
          !b64   = B.replicate 64 0x7f
          !b256  = B.replicate 256 0x7f
          !b1024 = B.replicate 1024 0x7f

          !p8    = PascalString b8
          !p32   = PascalString b32
          !p64   = PascalString b32
          !p256  = PascalString b256
          !p1024 = PascalString b1024

          !twoB8    = TwoBS b8 b8
          !twoB32   = TwoBS b32 b32
          !twoB64   = TwoBS b64 b64
          !twoB256  = TwoBS b256 b256
          !twoB1024 = TwoBS b1024 b1024

          !twoB8_1024  = TwoBS b8 b1024
          !twoB32_256  = TwoBS b32 b256
          !twoB1024_64 = TwoBS b1024 b64
          !twoB256_8   = TwoBS b256 b8

          !threeB8    = ThreeBS b8 b8 b8
          !threeB32   = ThreeBS b32 b32 b32
          !threeB64   = ThreeBS b64 b64 b64
          !threeB256  = ThreeBS b256 b256 b256
          !threeB1024 = ThreeBS b1024 b1024 b1024

          !lenthreeB1024 = ThreeBSWithLen threeB1024
          !lenthreeB1024IO = ThreeBSWithLenIO threeB1024

          !wordseq = WordSeq 12 14 15 16
          !wordseqs50 = WordSeqs $ replicate 50 wordseq
          !wordseqs1000 = WordSeqs $ replicate 1000 wordseq

