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

--{-# RULES "encode/TwoBS" forall b . Cereal.encode b = serializeTwoBS b #-}
serializeTwoBS (TwoBS b1 b2) = B.append b1 b2

benchAll x = [ benchBclass x, benchPacker x, benchCereal x, benchBuilder x, benchBinary x ]

main = defaultMain
    [ bgroup "pstr"
        [ bgroup "8" $ benchAll p8
        , bgroup "32" $ benchAll p32
        , bgroup "64" $ benchAll p64
        , bgroup "256" $ benchAll p256
        , bgroup "1024" $ benchAll p1024
        ]
    , bgroup "bs x 2"
        [ bgroup "8" $ benchAll twoB8
        , bgroup "32" $ benchAll twoB32
        , bgroup "64" $ benchAll twoB64
        , bgroup "256" $ benchAll twoB256
        , bgroup "1024" $ benchAll twoB1024
        ]
    , bgroup "bs + bs"
        [ bgroup "8+1024" $ benchAll twoB8_1024
        , bgroup "32+256" $ benchAll twoB32_256
        , bgroup "256+8" $ benchAll twoB256_8
        , bgroup "1024+64" $ benchAll twoB1024_64
        ]
    , bgroup "bs x 3"
        [ bgroup "8" $ benchAll threeB8
        , bgroup "32" $ benchAll threeB32
        , bgroup "64" $ benchAll threeB64
        , bgroup "256" $ benchAll threeB256
        , bgroup "1024" $ benchAll threeB1024
        ]
    , bgroup "len+3*1024"
        [ benchBclass lenthreeB1024
        , benchPacker lenthreeB1024
        , bench "bclass+io" $ nf implPutBasic lenthreeB1024IO
        , benchBuilder lenthreeB1024
        , benchCereal lenthreeB1024
        , benchBinary lenthreeB1024
        ]
    , bgroup "words x 4" $ benchAll wordseq
    , bgroup "words4 x 50" $ benchAll wordseqs50
    , bgroup "words4 x 1000" $ benchAll wordseqs1000
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

