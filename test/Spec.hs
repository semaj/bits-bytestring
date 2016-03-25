import            Data.Bits
import            Data.Bits.ByteString
import qualified  Data.ByteString as B
import            Test.Hspec
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

prop_LengthIdentityShiftL :: B.ByteString -> Int -> Bool
prop_LengthIdentityShiftL x i = (B.length x) == (B.length $ shiftL x i)

prop_LengthIdentityShiftR :: B.ByteString -> Int -> Bool
prop_LengthIdentityShiftR x i = (B.length x) == (B.length $ shiftR x i)

prop_LengthIdentityShift :: B.ByteString -> Int -> Bool
prop_LengthIdentityShift x i = (B.length x) == (B.length $ shiftR (shiftL x i) i)

prop_IdentityRotate :: B.ByteString -> Int -> Bool
prop_IdentityRotate x i = x == rotateR (rotateL x i) i

main :: IO ()
main = hspec $ do
  describe "AND" $ do
    context "Should have the following properties:" $ do
      it "Identity" $ property prop_IdentityAND
      it "Commutativity" $ property prop_CommutativityAND
      it "Associativity" $ property prop_AssociativityAND
  describe "OR" $ do
    context "Should have the following properties:" $ do
      it "Identity" $ property prop_IdentityOR
      it "Commutativity" $ property prop_CommutativityOR
      it "Associativity" $ property prop_AssociativityOR
  describe "XOR" $ do
    context "Should have the following properties:" $ do
      it "Identity" $ property prop_IdentityXOR
      it "Commutativity" $ property prop_CommutativityXOR
      it "Associativity" $ property prop_AssociativityXOR
  describe "Complement" $ do
    context "Should have the following properties:" $ do
      it "Identity" $ property prop_IdentityComplement
  describe "Shift" $ do
    context "Should have the following properties:" $ do
      it "Length should not change on ShiftL" $ property prop_LengthIdentityShiftL
      it "Length should not change on ShiftR" $ property prop_LengthIdentityShiftR
      it "Length should not change on Shift" $ property prop_LengthIdentityShift
  describe "Rotate" $ do
    context "Should have the following properties:" $ do
      it "Identity" $ property prop_IdentityRotate
