{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.HUnit
import Control.Applicative ((<$>))
import Control.Monad

import qualified Data.ByteString as B

import Data.Packer
import Data.Word

--instance Arbitrary a where

endiannessCases :: [ (String, (Word64 -> Packing ()), Unpacking Word64, B.ByteString, Word64) ]
endiannessCases =
    [ ("16LE", putWord16LE . fromIntegral, (fromIntegral <$> getWord16LE), B.pack [1,2], 0x0201)
    , ("16BE", putWord16BE . fromIntegral, (fromIntegral <$> getWord16BE), B.pack [2,1], 0x0201)
    , ("32LE", putWord32LE . fromIntegral, (fromIntegral <$> getWord32LE), B.pack [1,2,3,4], 0x04030201)
    , ("32BE", putWord32BE . fromIntegral, (fromIntegral <$> getWord32BE), B.pack [4,3,2,1], 0x04030201)
    , ("64LE", putWord64LE . fromIntegral, (fromIntegral <$> getWord64LE), B.pack [1,2,3,4,5,6,7,8], 0x0807060504030201)
    , ("64BE", putWord64BE . fromIntegral, (fromIntegral <$> getWord64BE), B.pack [8,7,6,5,4,3,2,1], 0x0807060504030201)
    ]

toEndianCase (n, pAct, gAct, bs, v) =
    [ testCase ("put" ++ n) (runPacking 8 (pAct v) @=? bs)
    , testCase ("get" ++ n) (runUnpacking gAct bs @=? v)
    ]

data DataAtom = W8    Word8
              | W16   Word16
              | W16BE Word16
              | W16LE Word16
              | W32   Word32
              | W32BE Word32
              | W32LE Word32
              | W64   Word64
              | W64BE Word64
              | W64LE Word64
              | Bytes B.ByteString
              deriving (Show,Eq)

newtype DataStream = DataStream [DataAtom]
    deriving (Show,Eq)

arbitraryBS =
        choose (0,257)
    >>= \sz -> replicateM sz (fromIntegral <$> (choose (0,255) :: Gen Int))
    >>= return . B.pack

instance Arbitrary DataAtom where
    arbitrary = oneof
        [ W8    <$> arbitrary
        , W16   <$> arbitrary
        , W16BE <$> arbitrary
        , W16LE <$> arbitrary
        , W32   <$> arbitrary
        , W32BE <$> arbitrary
        , W32LE <$> arbitrary
        , W64   <$> arbitrary
        , W64BE <$> arbitrary
        , W64LE <$> arbitrary
        , Bytes <$> arbitraryBS
        ]

instance Arbitrary DataStream where
    arbitrary = choose (0,2048)
            >>= \sz -> replicateM sz arbitrary
            >>= return . DataStream

packDataStream (DataStream atoms) = runPacking (foldl sumLen 0 atoms) (mapM_ process atoms)
    where process :: DataAtom -> Packing ()
          process (W8 w)    = putWord8 w
          process (W16 w)   = putWord16 w
          process (W32 w)   = putWord32 w
          process (W64 w)   = putWord64 w
          process (W16LE w) = putWord16LE w
          process (W32LE w) = putWord32LE w
          process (W64LE w) = putWord64LE w
          process (W16BE w) = putWord16BE w
          process (W32BE w) = putWord32BE w
          process (W64BE w) = putWord64BE w
          process (Bytes b) = putBytes b

          sumLen a (W8 _)    = a + 1
          sumLen a (W16 _)   = a + 2
          sumLen a (W16LE _) = a + 2
          sumLen a (W16BE _) = a + 2
          sumLen a (W32 _)   = a + 4
          sumLen a (W32LE _) = a + 4
          sumLen a (W32BE _) = a + 4
          sumLen a (W64 _)   = a + 8
          sumLen a (W64LE _) = a + 8
          sumLen a (W64BE _) = a + 8
          sumLen a (Bytes b) = a + B.length b

unpackDataStream :: DataStream -> B.ByteString -> DataStream
unpackDataStream (DataStream atoms) bs = DataStream $ runUnpacking (mapM process atoms) bs
    where process :: DataAtom -> Unpacking DataAtom
          process (W8 _)    = W8 <$> getWord8
          process (W16 _)   = W16 <$> getWord16
          process (W32 _)   = W32 <$> getWord32
          process (W64 _)   = W64 <$> getWord64
          process (W16LE _) = W16LE <$> getWord16LE
          process (W32LE _) = W32LE <$> getWord32LE
          process (W64LE _) = W64LE <$> getWord64LE
          process (W16BE _) = W16BE <$> getWord16BE
          process (W32BE _) = W32BE <$> getWord32BE
          process (W64BE _) = W64BE <$> getWord64BE
          process (Bytes b) = Bytes <$> getBytes (B.length b)
    
main :: IO ()
main = defaultMain
    [ testGroup "serialization"
        [ testGroup "basic cases"
            [ testCase "pack 4 bytes" (runPacking 4 (mapM_ putWord8 [1,2,3,4]) @=? B.pack [1,2,3,4])
            ]
        , testGroup "endianness cases" $ concatMap toEndianCase endiannessCases
        , testProperty "unpacking.packing=id" (\ds -> unpackDataStream ds (packDataStream ds) == ds)
        ]
    ]
