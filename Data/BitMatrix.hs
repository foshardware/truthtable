{-# LANGUAGE OverloadedStrings #-}

module Data.BitMatrix where

import Data.Bits
import Data.Foldable
import Data.Monoid
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Prelude hiding ((!!))

import Data.BitVector


(!!) :: V.Vector a -> Int -> a
(!!) = V.unsafeIndex

-- |    m0,m1,m2,..
--   r0 0x00000001
--   r1 0x00000002
--   ..
-- 
--  m0 <=> LSB
--
data BitMatrix = BitMatrix !Int (V.Vector BitVector)

instance Eq BitMatrix where
  BitMatrix m u == BitMatrix n v = m == n && u == v

-- | Inexpensive: right shifts, row operators, bitwise ands, word aligned parameters
--
instance Bits BitMatrix where
  bitSizeMaybe (BitMatrix c v) = Just $ c * V.length v
  isSigned _ = False

  BitMatrix c u .&. BitMatrix d v
    = BitMatrix (min c d) $ uncurry (.&.) <$> V.zip u v

  BitMatrix c u .|. BitMatrix d v
    | V.length u > V.length v
    = BitMatrix (max c d) . flip V.imap u
    $ \i ui -> if i >= V.length v then ui else ui .|. v!!i
  BitMatrix c u .|. BitMatrix d v
    | V.length u < V.length v
    = BitMatrix (max c d) . flip V.imap v
    $ \i vi -> if i >= V.length u then vi else u!!i .|. vi
  BitMatrix c u .|. BitMatrix d v
    = BitMatrix (max c d) $ uncurry (.|.) <$> V.zip u v
  
  BitMatrix c u `xor` BitMatrix d v
    | V.length u > V.length v
    = BitMatrix (max c d) . flip V.imap u
    $ \i ui -> if i >= V.length v then ui else ui `xor` v!!i
  BitMatrix c u `xor` BitMatrix d v
    | V.length u < V.length v
    = BitMatrix (max c d) . flip V.imap v
    $ \i vi -> if i >= V.length u then vi else u!!i `xor` vi
  BitMatrix c u `xor` BitMatrix d v
    = BitMatrix (max c d) $ uncurry xor <$> V.zip u v

  complement (BitMatrix c v)
    = BitMatrix c $ complementN c <$> v

  popCount (BitMatrix c v)
    = sum $ popCountN c <$> v

  testBit (BitMatrix c v) n
    = n <= c * V.length v && testBit (v !! div n c) (mod n c)

  bit n
    = BitMatrix wordSize . V.generate (divWord n + 1)
    $ \i -> if i /= divWord n then zeroBits else bit $ modWord n

  zeroBits = BitMatrix 0 V.empty

  clearBit (BitMatrix c v) n
    | n > c * V.length v
    = BitMatrix c v
  clearBit (BitMatrix c v) n
    | (m, k) <- divMod n c
    = BitMatrix c . flip V.imap v
    $ \i vi -> if i /= m then vi else vi .&. complementN k (bit k)

  shift (BitMatrix c v) 0 = BitMatrix c v 
  shift (BitMatrix c v) n = BitMatrix (c+n) $ flip shift n <$> v
  
  rotate (BitMatrix c v) 0 = BitMatrix c v 
  rotate (BitMatrix c v) n = BitMatrix c $ flip rotate n <$> v

rowShift :: BitMatrix -> Int -> BitMatrix
rowShift (BitMatrix c v) n | n > 0 = BitMatrix c $ V.replicate n zeroBits V.++ v
rowShift (BitMatrix c v) n | n < 0 = BitMatrix c $ V.drop (-n) v
rowShift bm _ = bm

rowShiftR :: BitMatrix -> Int -> BitMatrix
rowShiftR x n = rowShift x (-n)

rowShiftL :: BitMatrix -> Int -> BitMatrix
rowShiftL x n = rowShift x n

rowRotate :: BitMatrix -> Int -> BitMatrix
rowRotate (BitMatrix c v) n
  | n > 0
  , (v0, v1) <- V.splitAt (n `mod` V.length v) v
  = BitMatrix c $ v1 V.++ v0
rowRotate (BitMatrix c v) n
  | n < 0 
  , (v0, v1) <- V.splitAt (V.length v + (-n) `mod` V.length v) v
  = BitMatrix c $ v1 V.++ v0
rowRotate bm _ = bm

rowRotateR :: BitMatrix -> Int -> BitMatrix
rowRotateR x n = rowRotate x (-n)

rowRotateL :: BitMatrix -> Int -> BitMatrix
rowRotateL x n = rowRotate x n

rowSlice :: Int -> Int -> BitMatrix -> BitMatrix
rowSlice x n (BitMatrix c v) = BitMatrix c $ V.slice x n v

columnSlice :: Int -> Int -> BitMatrix -> BitMatrix
columnSlice _ 0 _ = BitMatrix 0 mempty
columnSlice 0 n (BitMatrix c v) | n >= c = BitMatrix c v
columnSlice x n (BitMatrix _ v) = BitMatrix n $ slice x n <$> v

-- | O(ms*rs)
transpose :: BitMatrix -> BitMatrix
transpose (BitMatrix c v)
  = BitMatrix (V.length v) . V.generate c
  $ \i -> V.ifoldl' (\n j a -> if testBit a i then setBit n j else n) zeroBits v


instance Show BitMatrix where
  show = unpack . toLazyText . bitMatrixBuilder

bitMatrixBuilder :: BitMatrix -> Builder
bitMatrixBuilder (BitMatrix c v) | elem 0 [c, V.length v] = "( )" 
bitMatrixBuilder (BitMatrix c v)
  = foldr (\e s -> "( " <> foldr (check e) mempty [0..(c-1)] <> " )\n" <> s) mempty v
  where
  check e j m | testBit e j = "×" <> m
  check _ _ m = singleton ' ' <> m
