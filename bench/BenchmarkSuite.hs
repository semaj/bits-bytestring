import            Criterion.Main
import            Data.Bits
import            Data.Bits.ByteString()
import qualified  Data.ByteString as B

zero :: B.ByteString
zero = B.replicate 64 0

bs512 :: B.ByteString
bs512 = B.replicate 64 255

bs1024 :: B.ByteString
bs1024 = B.replicate 128 255

benchBitwiseOperator :: (Int -> B.ByteString) -> [Benchmark]
benchBitwiseOperator f = fmap (\x -> bench (show x) $ whnf f x) ([128,256,512,1024] :: [Int])

main :: IO ()
main = defaultMain
  [ bgroup "AND" [ bench "512" $ whnf ((.&.) zero) bs512 ]
  , bgroup "OR" [ bench "512" $ whnf ((.|.) zero) bs512 ]
  , bgroup "XOR" [ bench "512" $ whnf (xor zero) bs512 ]
  , bgroup "shift" $ benchBitwiseOperator (shift bs1024)
  , bgroup "rotate" $ benchBitwiseOperator (rotate bs1024)
  ]
