{-# LANGUAGE OverloadedStrings #-}

module Data.BitMatrix where

import Data.Bits
import Data.Monoid
import Data.Foldable (foldr)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder
import Data.Vector hiding (singleton, slice, elem, foldr)
import qualified Data.Vector as Vector
import Prelude hiding ((++), take, drop, foldr, sum, length, splitAt, replicate, zip)

import Data.BitVector


(!!.) :: Vector a -> Int -> a
(!!.) = unsafeIndex

-- |    m0,m1,m2,..
--   r0 0x00000001
--   r1 0x00000002
--   ..
-- 
--  m0 <=> LSB
--
data BitMatrix = BitMatrix !Int (Vector BitVector)

instance Eq BitMatrix where
  BitMatrix m u == BitMatrix n v = m == n && u == v

-- | Inexpensive: right shifts, row operators, bitwise ands, word aligned parameters
--
instance Bits BitMatrix where
  bitSizeMaybe (BitMatrix c v) = Just $ c * length v
  isSigned _ = False

  BitMatrix c u .&. BitMatrix d v
    = BitMatrix (min c d) $ uncurry (.&.) <$> zip u v

  BitMatrix c u .|. BitMatrix d v
    | length u > length v
    = BitMatrix (max c d) . flip imap u
    $ \i ui -> if i >= length v then ui else ui .|. v!!.i
  BitMatrix c u .|. BitMatrix d v
    | length u < length v
    = BitMatrix (max c d) . flip imap v
    $ \i vi -> if i >= length u then vi else u!!.i .|. vi
  BitMatrix c u .|. BitMatrix d v
    = BitMatrix (max c d) $ uncurry (.|.) <$> zip u v
  
  BitMatrix c u `xor` BitMatrix d v
    | length u > length v
    = BitMatrix (max c d) . flip imap u
    $ \i ui -> if i >= length v then ui else ui `xor` v!!.i
  BitMatrix c u `xor` BitMatrix d v
    | length u < length v
    = BitMatrix (max c d) . flip imap v
    $ \i vi -> if i >= length u then vi else u!!.i `xor` vi
  BitMatrix c u `xor` BitMatrix d v
    = BitMatrix (max c d) $ uncurry xor <$> zip u v

  complement (BitMatrix c v)
    = BitMatrix c $ complementN c <$> v

  popCount (BitMatrix c v)
    = sum $ popCountN c <$> v

  testBit (BitMatrix c v) n
    = n <= c * length v && testBit (v !!. div n c) (mod n c)

  bit n
    = BitMatrix wordSize . generate (divWord n + 1)
    $ \i -> if i /= divWord n then zeroBits else bit $ modWord n

  zeroBits = BitMatrix 0 empty

  clearBit (BitMatrix c v) n
    | n > c * length v
    = BitMatrix c v
  clearBit (BitMatrix c v) n
    | (m, k) <- divMod n c
    = BitMatrix c . flip imap v
    $ \i vi -> if i /= m then vi else vi .&. complementN k (bit k)

  shift (BitMatrix c v) 0 = BitMatrix c v 
  shift (BitMatrix c v) n = BitMatrix (c+n) $ flip shift n <$> v
  
  rotate (BitMatrix c v) 0 = BitMatrix c v 
  rotate (BitMatrix c v) n = BitMatrix c $ flip rotate n <$> v

rowShift :: BitMatrix -> Int -> BitMatrix
rowShift (BitMatrix c v) n | n > 0 = BitMatrix c $ replicate n zeroBits ++ v
rowShift (BitMatrix c v) n | n < 0 = BitMatrix c $ drop (-n) v
rowShift bm _ = bm

rowShiftR :: BitMatrix -> Int -> BitMatrix
rowShiftR x n = rowShift x (-n)

rowShiftL :: BitMatrix -> Int -> BitMatrix
rowShiftL x n = rowShift x n

rowRotate :: BitMatrix -> Int -> BitMatrix
rowRotate (BitMatrix c v) n
  | n > 0
  , (v0, v1) <- splitAt (n `mod` length v) v
  = BitMatrix c $ v1 ++ v0
rowRotate (BitMatrix c v) n
  | n < 0 
  , (v0, v1) <- splitAt (length v + (-n) `mod` length v) v
  = BitMatrix c $ v1 ++ v0
rowRotate bm _ = bm

rowRotateR :: BitMatrix -> Int -> BitMatrix
rowRotateR x n = rowRotate x (-n)

rowRotateL :: BitMatrix -> Int -> BitMatrix
rowRotateL x n = rowRotate x n

rowSlice :: Int -> Int -> BitMatrix -> BitMatrix
rowSlice x n (BitMatrix c v) = BitMatrix c $ Vector.slice x n v

columnSlice :: Int -> Int -> BitMatrix -> BitMatrix
columnSlice _ 0 _ = BitMatrix 0 mempty
columnSlice 0 n (BitMatrix c v) | n >= c = BitMatrix c v
columnSlice x n (BitMatrix _ v) = BitMatrix n $ slice x n <$> v

-- | O(ms*rs)
transpose :: BitMatrix -> BitMatrix
transpose (BitMatrix c v)
  = BitMatrix (length v) . generate c
  $ \i -> ifoldl' (\n j a -> if testBit a i then setBit n j else n) zeroBits v


instance Show BitMatrix where
  show = unpack . toLazyText . bitMatrixBuilder

bitMatrixBuilder :: BitMatrix -> Builder
bitMatrixBuilder (BitMatrix c v) | elem 0 [c, length v] = "( )"
bitMatrixBuilder (BitMatrix c v)
  = foldr (\e s -> "( " <> foldr (check e) mempty [0..(c-1)] <> " )\n" <> s) mempty v
  where
  check e j m | testBit e j = "×" <> m
  check _ _ m = singleton ' ' <> m
