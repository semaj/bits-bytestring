{-# LANGUAGE OverloadedStrings #-}
module Data.Bits.ByteString
  ( bytestringAND
  , bytestringOR
  , bytestringXOR
  , bytestringComplement
  , bytestringShift
  , bytestringShiftR
  , bytestringShiftL
  , bytestringRotate
  , bytestringRotateR
  , bytestringRotateL
  , bytestringBitSize
  , bytestringBitSizeMaybe
  , bytestringIsSigned
  , bytestringTestBit
  , bytestringBit
  , bytestringPopCount
  ) where

import            Data.Bits
import qualified  Data.ByteString as B
import            Data.Word


bytestringAND :: B.ByteString -> B.ByteString -> B.ByteString
bytestringAND a b = B.pack $ B.zipWith (.&.) a b


bytestringOR :: B.ByteString -> B.ByteString -> B.ByteString
bytestringOR a b = B.pack $ B.zipWith (.|.) a b


bytestringXOR :: B.ByteString -> B.ByteString -> B.ByteString
bytestringXOR a b = B.pack $ B.zipWith xor a b


bytestringComplement :: B.ByteString -> B.ByteString
bytestringComplement = B.map complement


bytestringShift :: B.ByteString -> Int -> B.ByteString
bytestringShift x i
  | i < 0     = x `bytestringShiftR` (-i)
  | i > 0     = x `bytestringShiftL` i
  | otherwise = x


bytestringShiftR :: B.ByteString -> Int -> B.ByteString
bytestringShiftR bs 0 = bs
bytestringShiftR "" _ = B.empty
bytestringShiftR bs i =
    B.pack $ dropWhile (==0) $
      go (i `mod` 8) 0 (B.unpack bs)
  where
  go j w1 [] = []
  go j w1 (w2:wst) = (maskR j w1 w2) : go j w2 wst
  maskR i w1 w2 = (shiftL w1 (8-i)) .|. (shiftR w2 i)


bytestringShiftL :: B.ByteString -> Int -> B.ByteString
bytestringShiftL bs 0 = bs
bytestringShiftL "" _ = B.empty
bytestringShiftL bs i =
    B.pack $ dropWhile (==0)
      $ (go (i `mod` 8) 0 (B.unpack bs))
      ++ (replicate (i `div` 8) 0)
  where
  go j w1 [] = [shiftL w1 j]
  go j w1 (w2:wst) = (maskL j w1 w2) : go j w2 wst
  maskL i w1 w2 = (shiftL w1 i) .|. (shiftR w2 (8-i))


bytestringRotate :: B.ByteString -> Int -> B.ByteString
bytestringRotate x i
  | i < 0     = x `bytestringRotateR` (-i)
  | i > 0     = x `bytestringRotateL` i
  | otherwise = x


bytestringRotateR :: B.ByteString -> Int -> B.ByteString
bytestringRotateR x i = undefined


bytestringRotateL :: B.ByteString -> Int -> B.ByteString
bytestringRotateL x i = undefined


bytestringBitSize :: B.ByteString -> Int
bytestringBitSize x = undefined


bytestringBitSizeMaybe :: B.ByteString -> Maybe Int
bytestringBitSizeMaybe x = undefined


bytestringIsSigned :: B.ByteString -> Bool
bytestringIsSigned x = False


bytestringTestBit :: B.ByteString -> Int -> Bool
bytestringTestBit x i = undefined


bytestringBit :: Int -> B.ByteString
bytestringBit i = (bit $ mod i 8) `B.cons` (B.replicate (div i 8) (255 :: Word8))


bytestringPopCount :: B.ByteString -> Int
bytestringPopCount x = undefined
