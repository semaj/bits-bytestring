# bits-bytestring [![Build Status](https://travis-ci.org/oldmanmike/bits-bytestring.svg?branch=master)](https://travis-ci.org/oldmanmike/bits-bytestring)

A `ByteString` instance for the `Bits` typeclass, providing bitwise operators for n-bit blocks larger than `Word64`.

## Installation

To install bits-bytestring, use stack and add `bits-bytestring-1.0.0.0` to `extra-deps`:

```yaml
extra-deps: [bits-bytestring-1.0.0.0]
```

Then add `bits-bytestring to your cabal file or just run:

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

main = do
  let bs1 = SHA512.hash (B.pack [0..255])
  let bs2 = (B.replicate 64 0)
  let example1 = bs1 .&. bs2
  let example2 = bs1 .|. bs2
  let example3 = bs1 `xor` bs2
  print example1
  print example2
  print example3
```
