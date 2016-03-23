import            Data.Bits
import            Data.Bits.ByteString
import qualified  Data.ByteString as B
import            Test.QuickCheck hiding ((.&.))

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack (listOf arbitrary)

prop_IdentityAND :: B.ByteString -> Bool
prop_IdentityAND a = a == (a .&. a)

prop_CommutativityAND :: B.ByteString -> B.ByteString -> Bool
prop_CommutativityAND a b = (a .&. b) == (b .&. a)

prop_AssociativityAND :: B.ByteString -> B.ByteString -> B.ByteString -> Bool
prop_AssociativityAND a b c = (a .&. (b .&. c)) == ((a .&. b) .&. c)

prop_IdentityOR :: B.ByteString -> Bool
prop_IdentityOR a = a == (a .|. (B.replicate (B.length a) 0))

prop_CommutativityOR :: B.ByteString -> B.ByteString -> Bool
prop_CommutativityOR a b = (a .|. b) == (b .|. a)

prop_AssociativityOR :: B.ByteString -> B.ByteString -> B.ByteString -> Bool
prop_AssociativityOR a b c = (a .|. (b .|. c)) == ((a .|. b) .|. c)

prop_IdentityXOR :: B.ByteString -> Bool
prop_IdentityXOR a = a == (a `xor` (B.replicate (B.length a) 0))

prop_CommutativityXOR :: B.ByteString -> B.ByteString -> Bool
prop_CommutativityXOR a b = (a `xor` b) == (b `xor` a)

prop_AssociativityXOR :: B.ByteString -> B.ByteString -> B.ByteString -> Bool
prop_AssociativityXOR a b c = (a `xor` (b `xor` c)) == ((a `xor` b) `xor` c)

prop_IdentityComplement :: B.ByteString -> Bool
prop_IdentityComplement x = x == complement (complement x)

prop_IdentityShift :: B.ByteString -> Int -> Bool
prop_IdentityShift x i = x == shiftR (shiftL x i) i

prop_IdentityRotate :: B.ByteString -> Int -> Bool
prop_IdentityRotate x i = x == rotateR (rotateL x i) i

main :: IO ()
main = do
  quickCheck prop_IdentityAND
  quickCheck prop_CommutativityAND
  quickCheck prop_AssociativityAND
  quickCheck prop_IdentityOR
  quickCheck prop_CommutativityOR
  quickCheck prop_AssociativityOR
  quickCheck prop_IdentityXOR
  quickCheck prop_CommutativityXOR
  quickCheck prop_AssociativityXOR
  quickCheck prop_IdentityComplement
  quickCheck prop_IdentityRotate
