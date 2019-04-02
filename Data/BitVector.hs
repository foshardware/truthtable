{-# LANGUAGE OverloadedStrings #-}

module Data.BitVector where

import Data.Bits
import Data.Vector.Unboxed hiding (slice)
import Prelude hiding ((++), length, take, drop, replicate, splitAt, map, zip, all)


(!.) :: Vector Word -> Int -> Word
(!.) = unsafeIndex


newtype BitVector = BitVector (Vector Word)

wordSize :: Int
wordSize = finiteBitSize (maxBound :: Word)

modWord :: Int -> Int
modWord n | wordSize == 64 = n .&. 63
modWord n | wordSize == 32 = n .&. 31
modWord _ = undefined

divWord :: Int -> Int
divWord n | wordSize == 64 = shiftR n 6
divWord n | wordSize == 32 = shiftR n 5
divWord _ = undefined 

instance Eq BitVector where
  BitVector a == BitVector b
    | (a0, a1) <- splitAt (length b) a
    , (b0, b1) <- splitAt (length a) b
    = a0 == b0 && all (== 0) a1 && all (== 0) b1 

instance Bits BitVector where
  bitSizeMaybe (BitVector v) = Just $ length v * wordSize
  isSigned _ = False

  BitVector a .&. BitVector b
    = BitVector $ uncurry (.&.) `map` zip a b

  BitVector a .|. BitVector b
    | length a > length b
    = BitVector . flip imap a
    $ \i ai -> if i >= length b then ai else ai .|. b!i
  BitVector a .|. BitVector b
    | length a < length b
    = BitVector . flip imap b
    $ \i bi -> if i >= length a then bi else a!i .|. bi
  BitVector a .|. BitVector b
    = BitVector $ uncurry (.|.) `map` zip a b

  BitVector a `xor` BitVector b
    | length a > length b
    = BitVector . flip imap a
    $ \i ai -> if i >= length b then ai else ai `xor` b!i
  BitVector a `xor` BitVector b
    | length a < length b
    = BitVector . flip imap b
    $ \i bi -> if i >= length a then bi else a!i `xor` bi
  BitVector a `xor` BitVector b
    = BitVector $ uncurry xor `map` zip a b

  complement (BitVector v) = BitVector $ map complement v

  popCount (BitVector v) = foldl' (\n a -> n + popCount a) 0 v

  testBit (BitVector v) n = length v > divWord n && testBit (v ! divWord n) (modWord n)

  bit n
    = BitVector . generate (divWord n + 1)
    $ \i -> if i /= divWord n then 0 else bit $ modWord n

  zeroBits = BitVector empty

  clearBit b n = b .&. complementN n (bit n)

  shiftL (BitVector v) n
    | modWord n == 0
    = BitVector $ replicate (divWord n) 0 ++ v
  shiftL (BitVector v) n
    | m <- modWord n, k <- divWord n
    = BitVector . generate (length v + k)
    $ \i -> case compare 0 (i-k) of
      GT -> 0
      LT -> shiftR (v!(i-k-1)) (wordSize-m) .|. shiftL (v!(i-k)) m
      EQ -> shiftL (v!0) m
  shiftR (BitVector v) n
    | modWord n == 0
    = BitVector $ drop (divWord n) v
  shiftR (BitVector v) n
    | m <- modWord n, k <- divWord n
    = BitVector . generate (length v - k)
    $ \i -> case compare (i+k) (length v - 1) of
      LT -> shiftR (v!(i+k)) (wordSize-m) .|. shiftL (v!(i+k+1)) (wordSize-m)
      EQ -> shiftR (v!(i+k)) m
      GT -> undefined

  rotateL (BitVector v) n
    | modWord n == 0
    , (v0, v1) <- splitAt (divWord n) v
    = BitVector $ v1 ++ v0
  rotateL (BitVector v) n
    | m <- modWord n, k <- divWord n
    = BitVector . generate (length v)
    $ \i -> v!(mod (i+k) (length v)) .|. v!(mod (i+k) (length v))
  rotateR (BitVector v) n
    | modWord n == 0
    , (v0, v1) <- splitAt (length v - divWord n) v
    = BitVector $ v1 ++ v0

complementN :: Int -> BitVector -> BitVector
complementN 0 bv = bv
complementN n (BitVector v)
  = BitVector $ map complement v ++ replicate (divWord (n+wordSize-1) - length v) maxBound

popCountN :: Int -> BitVector -> Int 
popCountN n _ | n <= 0 = 0
popCountN n v | modWord n == 0 = popCount v
popCountN n (BitVector v)
  = popCount (v!(divWord n) .&. (shiftL 1 (modWord n) - 1))
  + popCount (BitVector $ take (divWord n) v)

slice :: Int -> Int -> BitVector -> BitVector
slice x n (BitVector v)
  | modWord n == 0
  , modWord x == 0
  = BitVector . take (divWord n)
  $ drop (divWord x) v 
slice x n (BitVector v)
  | (x+n) > wordSize * length v
  = slice x (wordSize * length v - x) (BitVector v)
slice x n (BitVector v)
  | modWord x == 0
  = BitVector . snoc (unsafeSlice (divWord x) (divWord n) v)
  $ v!(divWord x + divWord n) .&. (shiftL 1 (modWord n) - 1)
slice x n bv
  = slice 0 (x+n) bv `shiftR` x


instance Show BitVector where
  show bv@(BitVector v)
    = [ if testBit bv n then '1' else '0'
      | n <- [0..(wordSize * length v - 1)]
      ]

