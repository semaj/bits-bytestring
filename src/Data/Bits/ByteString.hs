{-# LANGUAGE OverloadedStrings #-}
module Data.Bits.ByteString where

import            Data.Bits
import qualified  Data.ByteString as B
import            Data.Word

instance Bits B.ByteString where

  (.&.) a b = B.pack $ B.zipWith (.&.) a b

  (.|.) a b = B.pack $ B.zipWith (.|.) a b

  xor a b = B.pack $ B.zipWith xor a b

  complement = B.map complement

  shift x i
    | i < 0     = x `shiftR` (-i)
    | i > 0     = x `shiftL` i
    | otherwise = x

  shiftR bs 0 = bs
  shiftR "" _ = B.empty
  shiftR bs i =
      B.pack $ dropWhile (==0) $
        go (i `mod` 8) 0 (B.unpack bs)
    where
    go j w1 [] = []
    go j w1 (w2:wst) = (maskR j w1 w2) : go j w2 wst
    maskR i w1 w2 = (shiftL w1 (8-i)) .|. (shiftR w2 i)


  shiftL bs 0 = bs
  shiftL "" _ = B.empty
  shiftL bs i =
      B.pack $ dropWhile (==0)
        $ (go (i `mod` 8) 0 (B.unpack bs))
        ++ (replicate (i `div` 8) 0)
    where
    go j w1 [] = [shiftL w1 j]
    go j w1 (w2:wst) = (maskL j w1 w2) : go j w2 wst
    maskL i w1 w2 = (shiftL w1 i) .|. (shiftR w2 (8-i))


  rotate x i
    | i < 0     = x `rotateR` (-i)
    | i > 0     = x `rotateL` i
    | otherwise = x


  rotateR bs 0 = bs
  rotateR bs i
      | B.length bs == 0 = B.empty
      | B.length bs == 1 = B.singleton (rotateR (bs `B.index` 0) i)
      | B.length bs > 1 = do
        let shiftedWords =
              B.append
                (B.drop (nWholeWordsToShift i) bs)
                (B.take (nWholeWordsToShift i) bs)
        let tmpShiftedBits = (shiftR shiftedWords (i `mod` 8))
        let rotatedBits = (shiftL (B.last shiftedWords) (8 - (i `mod` 8))) .|. (B.head tmpShiftedBits)
        rotatedBits `B.cons` (B.tail tmpShiftedBits)
    where
    nWholeWordsToShift n =  (B.length bs - (n `div` 8))


  rotateL bs 0 = bs
  rotateL bs i
      | B.length bs == 0 = B.empty
      | B.length bs == 1 = B.singleton (rotateL (bs `B.index` 0) i)
      | B.length bs > 1 = do
        let shiftedWords =
              B.append
                (B.take (nWholeWordsToShift i) bs)
                (B.drop (nWholeWordsToShift i) bs)
        let tmpShiftedBits = (shiftL shiftedWords (i `mod` 8))
        let rotatedBits = (shiftR (B.head shiftedWords) (8 - (i `mod` 8))) .|. (B.last tmpShiftedBits)
        (B.tail tmpShiftedBits) `B.snoc` rotatedBits
    where
    nWholeWordsToShift n = (B.length bs - (n `div` 8))


  bitSize x = 8 * B.length x


  bitSizeMaybe x = Just (8 * B.length x)


  isSigned x = False


  testBit x i = testBit (B.index x (B.length x - (i `div` 8) - 1)) (i `mod` 8)


  bit i = (bit $ mod i 8) `B.cons` (B.replicate (div i 8) (255 :: Word8))


  popCount x = sum $ map popCount $ B.unpack x
