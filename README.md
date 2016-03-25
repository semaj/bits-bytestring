# bits-bytestring [![Build Status](https://travis-ci.org/oldmanmike/bits-bytestring.svg?branch=master)](https://travis-ci.org/oldmanmike/bits-bytestring) [![Release](https://img.shields.io/hackage/v/bits-bytestring.svg)](https://hackage.haskell.org/package/bits-bytestring)

A `ByteString` instance for the `Bits` typeclass, providing bitwise operators for n-bit blocks larger than `Word64`.

## Installation

To install bits-bytestring, use stack and add `bits-bytestring-0.1.0.1` to `extra-deps` in `stack.yaml`:

```yaml
extra-deps: [bits-bytestring-0.1.0.1]
```

Then add `bits-bytestring` to your cabal file or just run:

```bash
stack install bits-bytestring
```

## Usage

`bits-bytestring` provides a `Bits` typeclass instance for `ByteString`, so all the bitwise operators from `Data.Bits` are available with `ByteString`s now. To access the instance, import `Data.Bits.ByteString`. Example usage could be:

```haskell
-- Minimal imports
import qualified Data.ByteString as B
import           Data.Bits
import           Data.Bits.ByteString

-- cryptohash example
import qualified Crypto.Hash.SHA512 as SHA512

main :: IO ()
main = do
  let bs1 = SHA512.hash (B.pack [0..255])
  let bs2 = (B.replicate 64 0)
  print $ bs1 .&. bs2
  print $ bs1 .|. bs2
  print $ bs1 `xor` bs2
  print $ shift bs1 5
  print $ rotate bs1 13
```

## Contributions

Pull requests and bug reports are welcome!

If you need to contact me, the following ways are best and in this order:

* Post to the Github repo with an issue or pull request
* Email me: mcarpenter.dev@gmail.com, oldmanmike.dev@gmail.com
* IRC: #haskell, #haskell-beginners, #haskell-game

- Michael Carpenter
