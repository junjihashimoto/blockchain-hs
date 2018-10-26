{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Sha256 where

import Data.Word
import Data.Bits
import Data.Map (Map,fromList)
import Text.Printf

data T8 = T8 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32
  deriving (Eq)

instance Show T8 where
  show (T8 v1 v2 v3 v4 v5 v6 v7 v8) = printf "%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x" v1 v2 v3 v4 v5 v6 v7 v8

data T16 =
  T16
  Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32
  Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32
  deriving (Eq)


instance Semigroup T8 where
  (<>) (T8 a b c d e f g h) (T8 a1 b1 c1 d1 e1 f1 g1 h1) = T8 (a+a1) (b+b1) (c+c1) (d+d1) (e+e1) (f+f1) (g+g1) (h+h1)

data T64 = T64
  { w0 :: Word32
  , w1 :: Word32
  , w2 :: Word32
  , w3 :: Word32
  , w4 :: Word32
  , w5 :: Word32
  , w6 :: Word32
  , w7 :: Word32
  , w8 :: Word32
  , w9 :: Word32
  , w10 :: Word32
  , w11 :: Word32
  , w12 :: Word32
  , w13 :: Word32
  , w14 :: Word32
  , w15 :: Word32
  , w16 :: Word32
  , w17 :: Word32
  , w18 :: Word32
  , w19 :: Word32
  , w20 :: Word32
  , w21 :: Word32
  , w22 :: Word32
  , w23 :: Word32
  , w24 :: Word32
  , w25 :: Word32
  , w26 :: Word32
  , w27 :: Word32
  , w28 :: Word32
  , w29 :: Word32
  , w30 :: Word32
  , w31 :: Word32
  , w32 :: Word32
  , w33 :: Word32
  , w34 :: Word32
  , w35 :: Word32
  , w36 :: Word32
  , w37 :: Word32
  , w38 :: Word32
  , w39 :: Word32
  , w40 :: Word32
  , w41 :: Word32
  , w42 :: Word32
  , w43 :: Word32
  , w44 :: Word32
  , w45 :: Word32
  , w46 :: Word32
  , w47 :: Word32
  , w48 :: Word32
  , w49 :: Word32
  , w50 :: Word32
  , w51 :: Word32
  , w52 :: Word32
  , w53 :: Word32
  , w54 :: Word32
  , w55 :: Word32
  , w56 :: Word32
  , w57 :: Word32
  , w58 :: Word32
  , w59 :: Word32
  , w60 :: Word32
  , w61 :: Word32
  , w62 :: Word32
  , w63 :: Word32
  } deriving (Eq)

instance Show T64 where
  show T64{..} =
    (printf "%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x\n"
      w0
      w1
      w2
      w3
      w4
      w5
      w6
      w7
      w8
      w9
      w10
      w11
      w12
      w13
      w14
      w15) <>
    (printf "%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x\n"
      w16
      w17
      w18
      w19
      w20
      w21
      w22
      w23
      w24
      w25
      w26
      w27
      w28
      w29
      w30
      w31
    ) <>
    (printf "%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x\n"
      w32
      w33
      w34
      w35
      w36
      w37
      w38
      w39
      w40
      w41
      w42
      w43
      w44
      w45
      w46
      w47
    ) <>
    (printf "%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x,%08x"
      w48
      w49
      w50
      w51
      w52
      w53
      w54
      w55
      w56
      w57
      w58
      w59
      w60
      w61
      w62
      w63
    )

(!) :: T64 -> Int -> Word32
(!) T64{..} i =
  case i of
    0 -> w0
    1 -> w1
    2 -> w2
    3 -> w3
    4 -> w4
    5 -> w5
    6 -> w6
    7 -> w7
    8 -> w8
    9 -> w9
    10 -> w10
    11 -> w11
    12 -> w12
    13 -> w13
    14 -> w14
    15 -> w15
    16 -> w16
    17 -> w17
    18 -> w18
    19 -> w19
    20 -> w20
    21 -> w21
    22 -> w22
    23 -> w23
    24 -> w24
    25 -> w25
    26 -> w26
    27 -> w27
    28 -> w28
    29 -> w29
    30 -> w30
    31 -> w31
    32 -> w32
    33 -> w33
    34 -> w34
    35 -> w35
    36 -> w36
    37 -> w37
    38 -> w38
    39 -> w39
    40 -> w40
    41 -> w41
    42 -> w42
    43 -> w43
    44 -> w44
    45 -> w45
    46 -> w46
    47 -> w47
    48 -> w48
    49 -> w49
    50 -> w50
    51 -> w51
    52 -> w52
    53 -> w53
    54 -> w54
    55 -> w55
    56 -> w56
    57 -> w57
    58 -> w58
    59 -> w59
    60 -> w60
    61 -> w61
    62 -> w62
    63 -> w63

setV :: T64 -> Int -> Word32 -> T64
setV t@T64{..} i v =
  case i of
    0 -> t{w0 = v}
    1 -> t{w1 = v}
    2 -> t{w2 = v}
    3 -> t{w3 = v}
    4 -> t{w4 = v}
    5 -> t{w5 = v}
    6 -> t{w6 = v}
    7 -> t{w7 = v}
    8 -> t{w8 = v}
    9 -> t{w9 = v}
    10 -> t{w10 = v}
    11 -> t{w11 = v}
    12 -> t{w12 = v}
    13 -> t{w13 = v}
    14 -> t{w14 = v}
    15 -> t{w15 = v}
    16 -> t{w16 = v}
    17 -> t{w17 = v}
    18 -> t{w18 = v}
    19 -> t{w19 = v}
    20 -> t{w20 = v}
    21 -> t{w21 = v}
    22 -> t{w22 = v}
    23 -> t{w23 = v}
    24 -> t{w24 = v}
    25 -> t{w25 = v}
    26 -> t{w26 = v}
    27 -> t{w27 = v}
    28 -> t{w28 = v}
    29 -> t{w29 = v}
    30 -> t{w30 = v}
    31 -> t{w31 = v}
    32 -> t{w32 = v}
    33 -> t{w33 = v}
    34 -> t{w34 = v}
    35 -> t{w35 = v}
    36 -> t{w36 = v}
    37 -> t{w37 = v}
    38 -> t{w38 = v}
    39 -> t{w39 = v}
    40 -> t{w40 = v}
    41 -> t{w41 = v}
    42 -> t{w42 = v}
    43 -> t{w43 = v}
    44 -> t{w44 = v}
    45 -> t{w45 = v}
    46 -> t{w46 = v}
    47 -> t{w47 = v}
    48 -> t{w48 = v}
    49 -> t{w49 = v}
    50 -> t{w50 = v}
    51 -> t{w51 = v}
    52 -> t{w52 = v}
    53 -> t{w53 = v}
    54 -> t{w54 = v}
    55 -> t{w55 = v}
    56 -> t{w56 = v}
    57 -> t{w57 = v}
    58 -> t{w58 = v}
    59 -> t{w59 = v}
    60 -> t{w60 = v}
    61 -> t{w61 = v}
    62 -> t{w62 = v}
    63 -> t{w63 = v}

toT64 :: T16 -> T64
toT64 (T16 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16) =
  T64
  v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

sha256init :: T8
sha256init =
  T8
  0x6a09e667
  0xbb67ae85
  0x3c6ef372
  0xa54ff53a
  0x510e527f
  0x9b05688c
  0x1f83d9ab
  0x5be0cd19

k :: T64
k =
  T64
  0x428a2f98  0x71374491  0xb5c0fbcf  0xe9b5dba5  0x3956c25b  0x59f111f1
  0x923f82a4  0xab1c5ed5  0xd807aa98  0x12835b01  0x243185be  0x550c7dc3
  0x72be5d74  0x80deb1fe  0x9bdc06a7  0xc19bf174  0xe49b69c1  0xefbe4786
  0x0fc19dc6  0x240ca1cc  0x2de92c6f  0x4a7484aa  0x5cb0a9dc  0x76f988da
  0x983e5152  0xa831c66d  0xb00327c8  0xbf597fc7  0xc6e00bf3  0xd5a79147
  0x06ca6351  0x14292967  0x27b70a85  0x2e1b2138  0x4d2c6dfc  0x53380d13
  0x650a7354  0x766a0abb  0x81c2c92e  0x92722c85  0xa2bfe8a1  0xa81a664b
  0xc24b8b70  0xc76c51a3  0xd192e819  0xd6990624  0xf40e3585  0x106aa070
  0x19a4c116  0x1e376c08  0x2748774c  0x34b0bcb5  0x391c0cb3  0x4ed8aa4a
  0x5b9cca4f  0x682e6ff3  0x748f82ee  0x78a5636f  0x84c87814  0x8cc70208
  0x90befffa  0xa4506ceb  0xbef9a3f7  0xc67178f2

ror32 :: Word32 -> Int -> Word32
ror32 word shift = (word `shiftR` shift) .|. (word `shiftL` (32-shift))

e0 x = ror32 x  2  `xor` ror32 x 13  `xor` ror32 x 22
e1 x = ror32 x  6  `xor` ror32 x 11  `xor` ror32 x 25
s0 x = ror32 x  7  `xor` ror32 x 18  `xor` (x `shiftR` 3)
s1 x = ror32 x 17  `xor` ror32 x 19  `xor` (x `shiftR` 10)

rotate' :: T8 -> T8
rotate' (T8 a b c d e f g h) = (T8 h a b c d e f g)

rotate1 :: T8 -> Word32 -> Word32 -> T8
rotate1 (T8 a b c d e f g h) k w =
  let t1 = h + e1 e + (g `xor` (e .&. (f `xor` g))) + k + w
      t2 = e0 a + ((a .&. b) .|. (c .&. (a .|. b)))
  in T8 a b c (d+t1) e f g (t1+t2)

rotate8 :: T8 -> T64 -> T8
rotate8 h w = foldl (\a' i -> rotate8' a' i) h [0,8..63]
  where
    rotate8' a0' i = foldl (\a j -> rotate' $ rotate1 a (k!(i + j)) (w!(i + j))) a0' [0..7]

sampleData :: T16
sampleData = T16 0x68656c6c 0x6f776f72 0x6c648000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000050

-- | sha256shuffle
--
-- >>> sha256shuffle sampleData
-- 68656c6c,6f776f72,6c648000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000050
-- 9b520559,9cedb18b,ae85aa42,6ed38687,6042a3f8,b388ee52,059b2cd1,0621f2e7,90e17640,75b495ea,049fe8b8,3897bd41,bcd31378,2ea1cb87,92327028,8ea4d3fb
-- f002fd6f,8f4bcfb2,eae97aaa,1dea27e6,a9a1a2ea,065028e0,2bb42689,d640505a,b6f40a92,0d667671,10f4583b,fa6d4a2a,e3aac519,462cb9a2,74c11804,904965e1
-- 33a44659,54cb577b,d7c13354,6bbabc98,7ac202b9,a35abd5a,b884ee5e,442b2f16,74b44df1,39d231aa,443e9bfc,7311f205,87b7877d,06813796,74aec04b,67bba45d
sha256shuffle :: T16 -> T64
sha256shuffle w16 = foldl (\ww i -> setV ww i (s1 (ww ! (i-2)) + (ww ! (i-7)) + s0 (ww ! (i-15)) + (ww ! (i-16)))) (toT64 w16) [16..63]


-- | sha256_do_chunk
--
-- >>> sha256_do_chunk sha256init sampleData
-- 936a185c,aaa266bb,9cbe981e,9e05cb78,cd732b0b,3280eb94,4412bb6f,8f8f07af
sha256_do_chunk :: T8 -> T16 -> T8
sha256_do_chunk h w16 = h <> (rotate8 h $ sha256shuffle w16)
