{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty (defaultMain, testGroup)

import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty (TestTree)
import Control.Monad
import Control.Exception

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

toEndianCase :: (Eq a, Show a) => (String, a -> Packing (), Unpacking a, B.ByteString, a) -> [TestTree]
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
              | F32LE Float
              | F32BE Float
              | F64LE Double
              | F64BE Double
              | Bytes B.ByteString
              deriving (Show,Eq)

newtype DataStream = DataStream [DataAtom]
    deriving (Show,Eq)

arbitraryBS :: Gen B.ByteString
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
        , F32LE <$> arbitrary
        , F32LE <$> arbitrary
        , F64BE <$> arbitrary
        , F64BE <$> arbitrary
        , Bytes <$> arbitraryBS
        ]

putAtom :: DataAtom -> Packing ()
putAtom (W8 w)    = putWord8 w
putAtom (W16 w)   = putWord16 w
putAtom (W32 w)   = putWord32 w
putAtom (W64 w)   = putWord64 w
putAtom (W16LE w) = putWord16LE w
putAtom (W32LE w) = putWord32LE w
putAtom (W64LE w) = putWord64LE w
putAtom (W16BE w) = putWord16BE w
putAtom (W32BE w) = putWord32BE w
putAtom (W64BE w) = putWord64BE w
putAtom (F32LE w) = putFloat32LE w
putAtom (F32BE w) = putFloat32BE w
putAtom (F64LE w) = putFloat64LE w
putAtom (F64BE w) = putFloat64BE w
putAtom (Bytes b) = putBytes b

getAtom :: DataAtom -> Unpacking DataAtom
getAtom (W8 _)    = W8    <$> getWord8
getAtom (W16 _)   = W16   <$> getWord16
getAtom (W32 _)   = W32   <$> getWord32
getAtom (W64 _)   = W64   <$> getWord64
getAtom (W16LE _) = W16LE <$> getWord16LE
getAtom (W32LE _) = W32LE <$> getWord32LE
getAtom (W64LE _) = W64LE <$> getWord64LE
getAtom (W16BE _) = W16BE <$> getWord16BE
getAtom (W32BE _) = W32BE <$> getWord32BE
getAtom (W64BE _) = W64BE <$> getWord64BE
getAtom (F32LE _) = F32LE <$> getFloat32LE
getAtom (F32BE _) = F32BE <$> getFloat32BE
getAtom (F64LE _) = F64LE <$> getFloat64LE
getAtom (F64BE _) = F64BE <$> getFloat64BE
getAtom (Bytes b) = Bytes <$> getBytes (B.length b)

getAtomLength :: DataAtom -> Int
getAtomLength (W8 _)    = 1
getAtomLength (W16 _)   = 2
getAtomLength (W16LE _) = 2
getAtomLength (W16BE _) = 2
getAtomLength (W32 _)   = 4
getAtomLength (W32LE _) = 4
getAtomLength (W32BE _) = 4
getAtomLength (F32LE _) = 4
getAtomLength (F32BE _) = 4
getAtomLength (W64 _)   = 8
getAtomLength (W64LE _) = 8
getAtomLength (W64BE _) = 8
getAtomLength (F64LE _) = 8
getAtomLength (F64BE _) = 8
getAtomLength (Bytes b) = B.length b

test_property_hole_pack_unpack :: DataAtom -> Bool
test_property_hole_pack_unpack a =
    a == r2
  where
    p1 = runPacker a
    r1 = runParser p1
    p2 = runPacker r1
    r2 = runParser p2

    runPacker :: DataAtom -> B.ByteString
    runPacker b = runPacking (getAtomLength a) (packer b)
    runParser :: B.ByteString -> DataAtom
    runParser = runUnpacking (getAtom a)

    packer :: DataAtom -> Packing ()
    packer (W8 w)    = putHoleWord8 >>= flip fillHole w
    packer (W16 w)   = putHoleWord16 >>= flip fillHole w
    packer (W32 w)   = putHoleWord32 >>= flip fillHole w
    packer (W64 w)   = putHoleWord64 >>= flip fillHole w
    packer (W16LE w) = putHoleWord16LE >>= flip fillHole w
    packer (W32LE w) = putHoleWord32LE >>= flip fillHole w
    packer (W64LE w) = putHoleWord64LE >>= flip fillHole w
    packer (W16BE w) = putHoleWord16BE >>= flip fillHole w
    packer (W32BE w) = putHoleWord32BE >>= flip fillHole w
    packer (W64BE w) = putHoleWord64BE >>= flip fillHole w
    packer (F32LE w) = putFloat32LE w
    packer (F32BE w) = putFloat32BE w
    packer (F64LE w) = putFloat64LE w
    packer (F64BE w) = putFloat64BE w
    packer (Bytes b) = putBytes b

instance Arbitrary DataStream where
    arbitrary = choose (0,2048)
            >>= \sz -> replicateM sz arbitrary
            >>= return . DataStream

packDataStream :: DataStream -> B.ByteString
packDataStream (DataStream atoms) = runPacking (foldl sumLen 0 atoms) (mapM_ putAtom atoms)
    where sumLen a at = a + getAtomLength at

unpackDataStream :: DataStream -> B.ByteString -> DataStream
unpackDataStream (DataStream atoms) bs = DataStream $ runUnpacking (mapM getAtom atoms) bs

newtype DataBytes = DataBytes B.ByteString
  deriving (Show, Eq)
instance Arbitrary DataBytes where
    arbitrary = DataBytes <$> arbitraryBS

test_property_hole :: DataBytes -> Bool
test_property_hole dbytes =
    dbytes == r2
  where
    p1 = runPacker dbytes
    r1 = runParser p1
    p2 = runPacker r1
    r2 = runParser p2

    runPacker :: DataBytes -> B.ByteString
    runPacker (DataBytes bs) = runPacking (B.length bs + 4) (packer bs)
    runParser :: B.ByteString -> DataBytes
    runParser = runUnpacking parser

    packer :: B.ByteString -> Packing ()
    packer bs = do
        hsize <- putHoleWord32LE
        putBytes bs
        fillHole hsize (fromIntegral $ B.length bs)
    parser :: Unpacking DataBytes
    parser = do
        size <- fromIntegral <$> getWord32LE
        DataBytes <$> getBytes size

assertException :: Exception e => String -> (e -> Maybe b) -> a -> IO ()
assertException msg filterE act =
    handleJust filterE (\_ -> return ()) (evaluate act >> assertFailure (msg ++ " didn't raise the proper exception"))


endOfInputCase :: [TestTree]
endOfInputCase =
    [ testCase "endOfInput False" (getEndOfInput getWord8 "as" @=? False)
    , testCase "endOfInput True" (getEndOfInput getWord16 "as" @=? True)
    ]
  where
    getEndOfInput :: Unpacking a -> B.ByteString -> Bool
    getEndOfInput getter bs =
        let inner = do
                _ <- getter
                endOfInput
        in runUnpacking inner bs


countRemainingCase :: [TestTree]
countRemainingCase =
    [ testCase "countRemaining 3" (getRemainingCount getWord8 "abcd" @=? 3)
    , testCase "countRemaining 2" (getRemainingCount getWord16 "abcd" @=? 2)
    , testCase "countRemaining 0" (getRemainingCount getWord32 "abcd" @=? 0)
    ]
  where
    getRemainingCount :: Unpacking a -> B.ByteString -> Int
    getRemainingCount getter bs =
        let inner = do
                _ <- getter
                countRemaining
        in runUnpacking inner bs


main :: IO ()
main = defaultMain $ testGroup "packer"
    [ testGroup "serialization"
        [ testGroup "basic cases"
            [ testCase "packing 4 bytes" (runPacking 4 (mapM_ putWord8 [1,2,3,4]) @=? B.pack [1,2,3,4])
            , testCase "packing out of bounds" (assertException "packing" (\(OutOfBoundPacking _ _) -> Just ())
                                                    (runPacking (1 :: Int) (mapM_ putWord8 [1,2])))
            , testCase "unpacking out of bounds" (assertException "unpacking" (\(OutOfBoundUnpacking _ _) -> Just ())
                                                    (runUnpacking (mapM_ (\_ -> getWord8 >> return ()) [1 :: Int,2]) (B.singleton 1)))
            , testCase "unpacking set pos before" (assertException "unpacking position" (\(OutOfBoundUnpacking _ _) -> Just ())
                                                    (runUnpacking (unpackSetPosition 2) (B.singleton 1)))
            , testCase "unpacking set pos after" (assertException "unpacking position" (\(OutOfBoundUnpacking _ _) -> Just ())
                                                    (runUnpacking (unpackSetPosition (-1)) (B.singleton 1)))
            , testCase "unpacking set pos end" (runUnpacking (unpackSetPosition 0) (B.empty) @=? ())
            , testCase "unpacking isolate" (runUnpacking (isolate 2 (getBytes 2) >>= \i -> getWord8 >>= \r -> return (i,r)) (B.pack [1,2,3]) @=? (B.pack [1,2], 3))
            , testCase "unpacking isolate out of bounds" $
                assertException "unpacking isolate" (\(OutOfBoundUnpacking _ _) -> Just ())
                    (runUnpacking (isolate 2 (getBytes 3)) (B.pack [1,2,3]))
            , testCase "unpacking isolate not consumed" $
                assertException "unpacking isolate" (\(IsolationNotFullyConsumed _ _) -> Just ())
                    (runUnpacking (isolate 3 (getBytes 2)) (B.pack [1,2,3]))

            ]
        , testGroup "endOfInput" endOfInputCase
        , testGroup "countRemaining" countRemainingCase
        , testGroup "endianness cases" $ concatMap toEndianCase endiannessCases
        , testProperty "unpacking.packing=id" (\ds -> unpackDataStream ds (packDataStream ds) == ds)
        , testProperty "holes" test_property_hole
        , testProperty "holes" test_property_hole_pack_unpack
        ]
    ]
