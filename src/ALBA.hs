{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ALBA where

import Control.Exception (bracket)
import Data.Bits (setBit, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Data.ByteString.Internal (toForeignPtr0)
import Data.Functor (void)
import qualified Data.List as List
import Data.Serialize (Serialize, encode, getWord64le, putWord64le, runGet, runPut)
import Data.Word (Word64)
import Debug.Trace (trace)
import Foreign (Ptr, Word8, countTrailingZeros, free, mallocBytes, peekArray, withForeignPtr)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (Gen, arbitrary, sized, vectorOf)

foreign import capi unsafe "blake2b.h blake2b256_hash" blake2b256_hash :: Ptr Word8 -> Int -> Ptr Word8 -> IO Int

-- | ALBA parameters.
--
-- The goal of ALBA is that given a set of elements $S_p$ known to the prover,
-- such that $|S_p| \geq n_p$, the prover can convince the verifier that $|S_p| \geq n_f$
-- with $n_f < n_p$.
data Params = Params
  { λ_sec :: Integer
  -- ^ Security parameter
  -- Controls the probability that `extract` returns a set of size less than `n_f`.
  -- 128 seems like a good value
  , λ_rel :: Integer
  -- ^ Verification parameter.
  -- Controls the probability that `verify` returns `True` when the proof is invalid.
  -- 128 seems like a good value
  , n_p :: Word64
  -- ^ Estimated size of "honest" parties set.
  , n_f :: Word64
  -- ^ Estimated size of "adversarial" parties set.
  }
  deriving (Show)

-- | Weight function for type `a`.
type W a = a -> Int

newtype Proof = Proof (Integer, [Bytes])
  deriving (Show, Eq)
  deriving newtype (Serialize)

newtype Hash = Hash ByteString
  deriving newtype (Eq)
  deriving (Show) via Bytes

instance Semigroup Hash where
  Hash a <> Hash b = hash $ a <> b

instance Monoid Hash where
  mempty = Hash ""

class Hashable a where
  hash :: a -> Hash

instance Hashable Integer where
  hash = hash . encode

instance Hashable ByteString where
  hash bytes =
    unsafePerformIO $ bracket (mallocBytes 32) free $ \out ->
      let (foreignPtr, len) = toForeignPtr0 bytes
       in withForeignPtr foreignPtr $ \ptr -> do
            void $ blake2b256_hash ptr len out
            Hash . BS.pack <$> peekArray 32 out

instance Hashable a => Hashable [a] where
  hash = foldMap hash

instance (Hashable a, Hashable b) => Hashable (a, b) where
  hash (a, b) = hash a <> hash b

newtype Bytes = Bytes ByteString
  deriving newtype (Hashable, Eq, Serialize)

instance Show Bytes where
  show (Bytes bs) = show $ Hex.encode bs

genItems :: Int -> Gen [Bytes]
genItems len = sized $ \n -> vectorOf n (Bytes . BS.pack <$> vectorOf len arbitrary)

-- | Output a proof `the set of elements known to the prover `s_p` has size greater than $n_f$.
prove :: Params -> [Bytes] -> Proof
prove params@Params{n_p} s_p =
  go (fromInteger u - 1) round0
 where
  round0 = [(t, [s_i], h_i) | s_i <- s_p, t <- [1 .. d], let tuple = (t, [s_i]), let h_i = hash tuple]

  (u, d, q) = computeParams params

  prob = ceiling $ 1 / q

  h1 :: Word64 -> (Integer, [Bytes], Hash) -> Bool
  h1 n (_, _, h) = h `oracle` n == 0

  go :: Int -> [(Integer, [Bytes], Hash)] -> Proof
  go 0 acc =
    let s_p'' = filter (h1 prob) acc
     in Proof $ head $ map (\(k, bs, _) -> (k, bs)) s_p''
  go n acc =
    let !s_p' = filter (h1 n_p) acc
        !s_p'' = [(t, s_i : s_j, h_i) | s_i <- s_p, (t, s_j, h_j) <- s_p', let !h_i = h_j <> hash s_i]
     in go (n - 1) s_p''

computeParams :: Params -> (Integer, Integer, Double)
computeParams Params{λ_rel, λ_sec, n_p, n_f} =
  (u, d, q)
 where
  e = exp 1

  loge :: Double
  loge = logBase 2 e

  u =
    ceiling $
      (fromIntegral λ_sec + logBase 2 (fromIntegral λ_rel) + 1 + logBase 2 loge)
        / logBase 2 (fromIntegral n_p / fromIntegral n_f)

  d = (2 * u * λ_rel) `div` floor loge

  q :: Double
  q = 2 * fromIntegral λ_rel / (fromIntegral d * loge)

modBS :: ByteString -> Integer -> Integer
modBS bs q =
  let n = fromBytes bs q
   in n `mod` q

fromBytes :: ByteString -> Integer -> Integer
fromBytes bs q = BS.foldl' (\acc b -> (acc * 256 + fromIntegral b) `mod` q) 0 bs

fromBytesLE :: ByteString -> Word64
fromBytesLE = either error id . runGet getWord64le

toBytesLE :: Word64 -> ByteString
toBytesLE = runPut . putWord64le

writeProof :: FilePath -> Proof -> IO Int
writeProof file proof = do
  let serialized = encode proof
  BS.writeFile file serialized
  pure $ BS.length serialized

-- | Compute a "random" oracle from a `ByteString` that's lower than some integer `n`.
--
-- from ALBA paper, Annex C, p.50:
--
-- H0 and H1 need to output a uniformly distributed integer in [np]
-- (or 1 with probability $1/[np]$, which can be handled by outputting a
-- random integer and checking if it is 0). If $n_p$ is a power of 2, we
-- are done. Else, set a failure bound $ε_{fail}$, set $k =
-- ⌈log2(n_p/ε_{fail})⌉$ , and set $d = ⌊2^k/n_p⌋$. Use H to produce a k-
-- bit string, interpret it as an integer i ∈ [0, 2^k − 1], fail if $i ≥
-- dn_p$ , and output $i \mod n_p$ otherwise. (Naturally, only the honest
-- prover and verifier will actually fail; dishonest parties can do
-- whatever they want.)
oracle :: Hash -> Word64 -> Word64
oracle (Hash bytes) n =
  if isPowerOf2 n
    then modPowerOf2 bytes n
    else modNonPowerOf2 bytes n

modNonPowerOf2 :: ByteString -> Word64 -> Word64
modNonPowerOf2 bytes n =
  if i >= d * n
    then error $ "failed: i = " <> show i <> ", d = " <> show d <> ", n = " <> show n <> ", k = " <> show k
    else i `mod` n
 where
  k :: Word64 = ceiling $ logBase 2 (fromIntegral n / εFail)
  d = 2 ^ k `div` n
  i = modPowerOf2 bytes (2 ^ k)

εFail :: Double
εFail = 1e-10

modPowerOf2 :: ByteString -> Word64 -> Word64
modPowerOf2 bytes n =
  let r = fromBytesLE $ BS.take 8 bytes
   in (n - 1) .&. r

isPowerOf2 :: Word64 -> Bool
isPowerOf2 n =
  let q :: Word64 = truncate $ logBase 2 (fromIntegral n :: Double)
   in 2 ^ q == n

-- | Verify `Proof` that the set of elements known to the prover `s_p` has size greater than $n_f$.
verify :: Params -> Proof -> Bool
verify params@Params{n_p} (Proof (d, bs)) =
  let (u, _, q) = computeParams params

      fo item (0, _, []) =
        let h = hash (d, [item])
         in (oracle h n_p, h, [item])
      fo item (0, prev_h, acc) =
        let prf = item : acc
            h = prev_h <> hash item
            prob = ceiling $ 1 / q
            m =
              if length prf < length bs
                then oracle h n_p
                else oracle h prob
         in (m, h, item : acc)
      fo _ (k, n, acc) = (k, n, acc)

      fst3 (a, _, _) = a
   in length bs == fromInteger u
        && ((== 0) . fst3) (foldr fo (0, Hash "", []) bs)
