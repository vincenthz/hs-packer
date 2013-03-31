{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Control.Applicative
import Data.Packer as Packer
import Data.Word
import Control.DeepSeq
import qualified Data.ByteString as B

data WordSeq = WordSeq {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word16
                       {-# UNPACK #-} !Word16
                       {-# UNPACK #-} !Word64
    deriving (Show,Eq)

instance NFData WordSeq where
    rnf (WordSeq a b c d) = a `seq` b `seq` c `seq` d `seq` ()

bsLen1000 = runPacking 4 (Packer.putWord32LE 1000)

main = do
    let !b = B.append bsLen1000 (B.replicate (16*1000) 0)
    let !y = Packer.runUnpacking get b
    putStrLn $ show y
    where
        get = Packer.getWord32LE >>= \n -> replicateM (fromIntegral n) (WordSeq <$> Packer.getWord32LE <*> Packer.getWord16LE <*> Packer.getWord16LE <*> Packer.getWord64LE)

