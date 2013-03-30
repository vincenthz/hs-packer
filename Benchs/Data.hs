{-# LANGUAGE BangPatterns #-}
module Data where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import Control.DeepSeq
import Control.Applicative
import Control.Monad

import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Data.Monoid
import Data.Bits
import qualified Data.ByteString.Lazy.Builder as Builder

import qualified Data.Serialize.Put as Cereal
import qualified Data.Serialize.Get as Cereal
import qualified Data.Serialize as Cereal

import qualified Data.Binary.Put as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary as Binary

import qualified Data.Packer as Packer

import Impl

isl i n = fromIntegral i `shiftL` n
isr i n = fromIntegral (i `shiftR` n)

data TwoBS = TwoBS B.ByteString B.ByteString

instance NFData TwoBS where
    rnf (TwoBS a b) = a `seq` b `seq` ()

data ThreeBS = ThreeBS B.ByteString B.ByteString B.ByteString

instance NFData ThreeBS where
    rnf (ThreeBS a b c) = a `seq` b `seq` c `seq` ()

newtype ThreeBSWithLen = ThreeBSWithLen ThreeBS

instance NFData ThreeBSWithLen where
    rnf (ThreeBSWithLen b) = deepseq b ()

newtype ThreeBSWithLenIO = ThreeBSWithLenIO ThreeBS

instance NFData ThreeBSWithLenIO where
    rnf (ThreeBSWithLenIO b) = deepseq b ()

data WordSeq = WordSeq {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word16
                       {-# UNPACK #-} !Word16
                       {-# UNPACK #-} !Word64

instance NFData WordSeq where
    rnf (WordSeq a b c d) = a `seq` b `seq` c `seq` d `seq` ()

newtype WordSeqs = WordSeqs [WordSeq]

instance NFData WordSeqs where
    rnf (WordSeqs l) = deepseq l ()

newtype PascalString = PascalString B.ByteString

instance NFData PascalString where
    rnf (PascalString b) = b `seq` ()

----------------------------------------------

instance ImplBasic TwoBS where
    implPutBasic (TwoBS b1 b2) = B.append b1 b2
    implGetBasic = undefined

instance ImplBuilder TwoBS where
    implPutBuilder (TwoBS b1 b2) = Builder.toLazyByteString (Builder.byteString b1 `mappend` Builder.byteString b2)

instance ImplPacker TwoBS where
    implPutPacker (TwoBS b1 b2) = Packer.runPacking (B.length b1 + B.length b2) (Packer.putBytes b1 >> Packer.putBytes b2)
    implGetPacker = undefined

instance Cereal.Serialize TwoBS where
    put (TwoBS b1 b2) = Cereal.putByteString b1 >> Cereal.putByteString b2
    get = undefined

instance Binary.Binary TwoBS where
    put (TwoBS a b) = Binary.putByteString a >> Binary.putByteString b
    get = undefined

instance ImplBasic ThreeBS where
    implPutBasic (ThreeBS b1 b2 b3) = B.concat [b1,b2,b3]
    implGetBasic = undefined

instance ImplBuilder ThreeBS where
    implPutBuilder (ThreeBS b1 b2 b3) = Builder.toLazyByteString $ mconcat [Builder.byteString b1,Builder.byteString b2,Builder.byteString b3]

instance ImplPacker ThreeBS where
    implPutPacker (ThreeBS b1 b2 b3) = Packer.runPacking (B.length b1 + B.length b2 + B.length b3)
                           (Packer.putBytes b1 >> Packer.putBytes b2 >> Packer.putBytes b3)
    implGetPacker = undefined

instance Cereal.Serialize ThreeBS where
    put (ThreeBS b1 b2 b3) = Cereal.putByteString b1 >> Cereal.putByteString b2 >> Cereal.putByteString b3
    get = undefined

instance Binary.Binary ThreeBS where
    put (ThreeBS a b c) = Binary.putByteString a >> Binary.putByteString b >> Binary.putByteString c
    get = undefined

instance ImplBasic ThreeBSWithLen where
   implPutBasic (ThreeBSWithLen tbs) =  
        let (ThreeBS a b c) = tbs
            len             = B.length a + B.length b + B.length c
            getAt pos       = B.singleton $ fromIntegral (len `shiftR` pos)
            lenEncoded      = B.concat [getAt 24, getAt 16, getAt 8, B.singleton (fromIntegral len)]
         in B.concat [lenEncoded,a,b,c]
   implGetBasic = undefined

instance ImplPacker ThreeBSWithLen where
    implPutPacker (ThreeBSWithLen (ThreeBS b1 b2 b3)) =
        Packer.runPacking (4 + len) $ do Packer.putWord32 (fromIntegral len)
                                         Packer.putBytes b1
                                         Packer.putBytes b2
                                         Packer.putBytes b3
        where len = B.length b1 + B.length b2 + B.length b3
    implGetPacker _ = undefined

-- just a mutable bytestring test using unsafeCreate and direct poking/memcpy.
instance ImplBasic ThreeBSWithLenIO where
    implPutBasic (ThreeBSWithLenIO tbs) = B.unsafeCreate (4+len) $ \ptr -> do
        poke (castPtr ptr) (fromIntegral len :: Word32)
        getBsPtr a (\srcPtr -> B.memcpy (ptr `plusPtr` 4) srcPtr (B.length a))
        getBsPtr b (\srcPtr -> B.memcpy (ptr `plusPtr` (4+B.length a)) srcPtr (B.length b))
        getBsPtr c (\srcPtr -> B.memcpy (ptr `plusPtr` (4+B.length a+B.length b)) srcPtr (B.length c))
        where (ThreeBS a b c) = tbs
              len             = B.length a + B.length b + B.length c
              getBsPtr (B.PS fptr ofs _) f = withForeignPtr fptr (\ptr -> f (ptr `plusPtr` ofs))
    implGetBasic = undefined

instance ImplBuilder ThreeBSWithLen where
    implPutBuilder (ThreeBSWithLen tbs) =
        let (ThreeBS a b c) = tbs
            len             = B.length a + B.length b + B.length c
         in Builder.toLazyByteString (mconcat [Builder.word32LE (fromIntegral len), Builder.byteString a, Builder.byteString b, Builder.byteString c])

instance Cereal.Serialize ThreeBSWithLen where
    put (ThreeBSWithLen tbs) = do
        let (ThreeBS a b c) = tbs
        let len             = B.length a + B.length b + B.length c
        Cereal.putWord32le (fromIntegral len)
        Cereal.putByteString a >> Cereal.putByteString b >> Cereal.putByteString c
    get = undefined

instance Binary.Binary ThreeBSWithLen where
    put (ThreeBSWithLen tbs) = do
        let (ThreeBS a b c) = tbs
        let len             = B.length a + B.length b + B.length c
        Binary.putWord32le (fromIntegral len)
        Binary.putByteString a >> Binary.putByteString b >> Binary.putByteString c
    get = undefined

instance ImplBasic WordSeq where
    implPutBasic (WordSeq a b c d) =
        B.concat [B.replicate 4 0,B.replicate 2 0,B.replicate 2 0,B.replicate 8 0]
    implGetBasic bs =
        case B.unpack bs of
            [a1,a2,a3,a4,b1,b2,c1,c2,d1,d2,d3,d4,d5,d6,d7,d8] ->
                WordSeq (isl a1 0 .|. isl a2 8 .|. isl a3 16 .|. isl a4 24)
                        (isl b1 0 .|. isl b2 8)
                        (isl c1 0 .|. isl c2 8)
                        (isl d1 0 .|. isl d2 8 .|. isl d3 16 .|. isl d4 24 .|. isl d5 32 .|. isl d6 40 .|. isl d7 48 .|. isl d8 56)
            _ -> error "get basic WordSeq"

instance ImplPacker WordSeq where
    implPutPacker (WordSeq a b c d) = Packer.runPacking 16 $ do
        Packer.putWord32LE a
        Packer.putWord16LE b
        Packer.putWord16LE c
        Packer.putWord64LE d
    implGetPacker = Packer.runUnpacking $ do
        WordSeq <$> Packer.getWord32LE
                <*> Packer.getWord16LE
                <*> Packer.getWord16LE
                <*> Packer.getWord64LE

instance ImplBuilder WordSeq where
    implPutBuilder (WordSeq a b c d) =
         Builder.toLazyByteString $ mconcat
            [Builder.word32LE a
            ,Builder.word16LE b
            ,Builder.word16LE c
            ,Builder.word64LE d
            ]

instance Cereal.Serialize WordSeq where
    put (WordSeq a b c d) = Cereal.putWord32le a >> Cereal.putWord16le b >> Cereal.putWord16le c >> Cereal.putWord64le d
    get = WordSeq <$> Cereal.getWord32le 
                  <*> Cereal.getWord16le
                  <*> Cereal.getWord16le
                  <*> Cereal.getWord64le

instance Binary.Binary WordSeq where
    put (WordSeq a b c d) = Binary.putWord32le a >> Binary.putWord16le b >> Binary.putWord16le c >> Binary.putWord64le d
    get = WordSeq <$> Binary.getWord32le 
                  <*> Binary.getWord16le
                  <*> Binary.getWord16le
                  <*> Binary.getWord64le

instance ImplPacker WordSeqs where
    implPutPacker (WordSeqs l) =
        Packer.runPacking (4 + 16 * len) (Packer.putWord32LE (fromIntegral len) >> mapM_ loop l)
        where loop (WordSeq a b c d) = do
                    Packer.putWord32LE a
                    Packer.putWord16LE b
                    Packer.putWord16LE c
                    Packer.putWord64LE d
              len = length l
    implGetPacker = WordSeqs <$> Packer.runUnpacking (Packer.getWord32LE >>= \n -> replicateM (fromIntegral n) (WordSeq <$> Packer.getWord32LE <*> Packer.getWord16LE <*> Packer.getWord16LE <*> Packer.getWord64LE))

instance ImplBasic WordSeqs where
    implPutBasic (WordSeqs l) = B.concat (blen : concatMap tb l)
        where tb (WordSeq a b c d) = [B.replicate 4 0,B.replicate 2 0,B.replicate 2 0,B.replicate 8 0]
              blen = B.pack [len `isr` 0, len `isr` 8, len `isr` 16, len `isr` 24]
              len  = length l

    implGetBasic bs =
        let (blen,bs1) = B.splitAt 4 bs in
        let len = case B.unpack blen of
                    [a1,a2,a3,a4] -> (isl a1 0 .|. isl a2 8 .|. isl a3 16 .|. isl a4 24) in
        WordSeqs (loop len bs1)
        where loop :: Int -> B.ByteString -> [WordSeq]
              loop 0   _ = []
              loop len b =
                let (b1,b2) = B.splitAt 16 bs in
                implGetBasic b1 : loop (len-1) b2

instance ImplBuilder WordSeqs where
    implPutBuilder (WordSeqs l) = Builder.toLazyByteString $ mconcat (loop l)
        where loop [] = []
              loop ((WordSeq a b c d):xs) = Builder.word32LE a:Builder.word16LE b:Builder.word16LE c:Builder.word64LE d:loop xs

instance Cereal.Serialize WordSeqs where
    put (WordSeqs l) = Cereal.putWord32le (fromIntegral $ length l) >> mapM_ loop l
        where loop (WordSeq a b c d) = Cereal.putWord32le a >> Cereal.putWord16le b >> Cereal.putWord16le c >> Cereal.putWord64le d
    get = Cereal.getWord32le >>= \n -> WordSeqs <$> replicateM (fromIntegral n) (WordSeq <$> Cereal.getWord32le <*> Cereal.getWord16le <*> Cereal.getWord16le <*> Cereal.getWord64le)

instance Binary.Binary WordSeqs where
    put (WordSeqs l) = Binary.putWord32le (fromIntegral $ length l) >> mapM_ loop l
        where loop (WordSeq a b c d) = Binary.putWord32le a >> Binary.putWord16le b >> Binary.putWord16le c >> Binary.putWord64le d
    get = Binary.getWord32le >>= \n -> WordSeqs <$> replicateM (fromIntegral n) (WordSeq <$> Binary.getWord32le <*> Binary.getWord16le <*> Binary.getWord16le <*> Binary.getWord64le)

instance ImplBasic PascalString where
    implPutBasic (PascalString b) =
        let !len = B.length b
            blen = B.pack [len `isr` 0, len `isr` 8, len `isr` 16, len `isr` 24]
         in B.concat [blen, b]
    implGetBasic bs =
        let (blen,brem) = B.splitAt 4 bs
         in case B.unpack blen of
                [a1,a2,a3,a4] -> let len = (isl a1 0 .|. isl a2 8 .|. isl a3 16 .|. isl a4 24)
                                  in PascalString $ fst $ B.splitAt len brem
                _ -> error "cannot get len"

instance ImplBuilder PascalString where
    implPutBuilder (PascalString b) = Builder.toLazyByteString (mconcat [Builder.word32LE (fromIntegral $ B.length b), Builder.byteString b])

instance ImplPacker PascalString where
    implPutPacker (PascalString b) = Packer.runPacking (len + 4) (Packer.putWord32LE (fromIntegral len) >> Packer.putBytes b)
                        where !len = B.length b
    implGetPacker = Packer.runUnpacking $ do Packer.getWord32LE >>= \len -> (PascalString <$> Packer.getBytes (fromIntegral len))

instance Cereal.Serialize PascalString where
    put (PascalString b) = Cereal.putWord32le (fromIntegral $ B.length b) >> Cereal.putByteString b
    get = Cereal.getWord32le >>= \len -> (PascalString <$> Cereal.getByteString (fromIntegral len))

instance Binary.Binary PascalString where
    put (PascalString b) = Binary.putWord32le (fromIntegral $ B.length b) >> Binary.putByteString b
    get = Binary.getWord32le >>= \len -> (PascalString <$> Binary.getByteString (fromIntegral len))
