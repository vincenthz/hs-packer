module Impl where

import Criterion.Main
import Control.DeepSeq

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Serialize.Put as Cereal
import qualified Data.Serialize as Cereal
import qualified Data.Binary.Put as Binary
import qualified Data.Binary as Binary
import qualified Data.Packer as Packer

class ImplBasic a where
    implPutBasic :: a -> B.ByteString
    implGetBasic :: B.ByteString -> a

class ImplBuilder a where
    implPutBuilder :: a -> L.ByteString

class ImplPacker a where
    implPutPacker :: a -> B.ByteString
    implGetPacker :: B.ByteString -> a

instance ImplBasic B.ByteString where
    implPutBasic bs = bs
    implGetBasic bs = bs

instance ImplBuilder B.ByteString where
    implPutBuilder bs = Builder.toLazyByteString (Builder.byteString bs)

instance ImplPacker B.ByteString where
    implPutPacker bs = Packer.runPacking (B.length bs) (Packer.putBytes bs)
    implGetPacker bs = Packer.runUnpacking (Packer.getBytes (B.length bs)) bs

benchBclass :: ImplBasic a => a -> Benchmark
benchBclass = bench "bclass" . nf implPutBasic

benchBuilder :: ImplBuilder a => a -> Benchmark
benchBuilder = bench "builder" . nf implPutBuilder

benchPacker :: ImplPacker a => a -> Benchmark
benchPacker = bench "packer" . nf implPutPacker

benchCereal :: Cereal.Serialize a => a -> Benchmark
benchCereal = bench "cereal" . nf Cereal.encode

benchBinary :: Binary.Binary a => a -> Benchmark
benchBinary = bench "binary" . nf Binary.encode

benchBclassGet :: (NFData a, ImplBasic a) => a -> ByteString -> Benchmark
benchBclassGet a bs = bench "bclass" $ nf (runDecode a) bs
    where runDecode :: ImplBasic a => a -> ByteString -> a
          runDecode _ bs = implGetBasic bs

benchPackerGet :: (NFData a, ImplPacker a) => a -> ByteString -> Benchmark
benchPackerGet a bs = bench "packer" $ nf (runDecode a) bs
    where runDecode :: ImplPacker a => a -> ByteString -> a
          runDecode _ bs = implGetPacker bs

benchCerealGet :: (NFData a, Cereal.Serialize a) => a -> ByteString -> Benchmark
benchCerealGet a bs = bench "cereal" $ nf (runDecode a) bs
    where runDecode :: Cereal.Serialize a => a -> ByteString -> a
          runDecode _ bs = either error id $ Cereal.decode bs

benchBinaryGet :: (NFData a, Binary.Binary a) => a -> ByteString -> Benchmark
benchBinaryGet a bs = bench "binary" $ nf (runDecode a) (L.fromChunks [bs])
    where runDecode :: Binary.Binary a => a -> L.ByteString -> a
          runDecode _ bs = Binary.decode bs
