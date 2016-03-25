import            Criterion.Main
import            Data.Bits
import            Data.Bits.ByteString
import qualified  Data.ByteString as B

bs512 :: B.ByteString
bs512 = B.replicate 64 255

benchBitwiseOperator :: (Enum a, Show a, Bits b) => (a -> b) -> a -> a -> [Benchmark]
benchBitwiseOperator f start finish = fmap (\x -> bench (show x) $ whnf f x) [start..finish]

main = defaultMain [
  bgroup "shift" $ benchBitwiseOperator (shift bs512) 0 512
  ]
