Packer: fast strict serialization in haskell
============================================

Packer is originally an experiment when benchmarking other serialization
solution, namely cereal and binary. It turns out that the experiment yields
interesting result performance wise, and also allowed different use cases.

Packer provides a way to de-serialize and serialize data, from/to bytestring.
The usage is very similar to binary and cereal runPut/runGet, except that it
doesn't allow incremental feeding.

Holes
-----

The interface allows to create holes during the packing; this is useful to
have a more C like API where some fields are computed a-posteriori:

    putF = do
        crc32  <- putHoleWord32LE
        crcVal <- foldM (\acc w -> putWord32LE w >> addCRC acc w) 0 [1,2,3]
        fillHole crc32 crcVal
