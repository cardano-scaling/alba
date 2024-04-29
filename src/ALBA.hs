{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ALBA where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Bits (FiniteBits, countLeadingZeros, finiteBitSize, (.&.), (.<<.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Data.ByteString.Internal (unsafeCreate)
import Data.Serialize (Serialize, decode, encode, getWord64le, putWord64le, runGet, runPut)
import Data.Word (Word64)
import Foreign (Ptr, Word8, castPtr, countTrailingZeros)
import Foreign.C (errnoToIOError, getErrno)
import GHC.IO.Exception (ioException)
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

newtype Hash where
  Hash :: ByteString -> Hash
  deriving newtype (Eq, NFData)
  deriving (Show) via Bytes

instance Semigroup Hash where
  Hash a <> Hash b =
    {-# SCC hash_semigroup #-}
    hash $ a <> b

instance Monoid Hash where
  mempty = Hash ""

class Hashable a where
  hash :: a -> Hash

instance Hashable Integer where
  hash = hash . encode

instance Hashable ByteString where
  hash bytes =
    {-# SCC hash_bs #-}
    Hash <$> unsafeCreate 32 $ \outptr ->
      BS.useAsCStringLen bytes $ \(inptr, inputlen) -> do
        res <- blake2b256_hash (castPtr inptr) inputlen outptr
        unless (res == 0) $ do
          errno <- getErrno
          ioException $ errnoToIOError "blake2b256_hash" errno Nothing Nothing

instance Hashable a => Hashable [a] where
  hash =
    {-# SCC hash_list #-}
    foldMap hash

instance (Hashable a, Hashable b) => Hashable (a, b) where
  hash (a, b) =
    {-# SCC hash_pair #-}
    hash a <> hash b

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
  preHash = zip s_p $ map (\bs -> let h = hash bs in (h, h `oracle` n_p)) s_p

  round0 =
    [ (t, [s_i], h_0, n_p0)
    | (s_i, (h, l)) <- preHash
    , t <- [1 .. d]
    , let !h_0 = hash t <> h
    , let n_p0 = h_0 `oracle` n_p
    , l == n_p0
    ]

  (u, d, q) = computeParams params

  prob_q = ceiling $ 1 / q

  go :: Int -> [(Integer, [Bytes], Hash, Word64)] -> Proof
  go 0 acc =
    Proof $ head $ [(k, bs) | (k, bs, h, _) <- acc, h `oracle` prob_q == 0]
  go n acc =
    let !s_p'' =
          {-# SCC s_p'' #-}
          [ (t, s_i : s_j, h_i, h_i `oracle` n_p)
          | (s_i, (h_si, n_pi)) <- preHash
          , (t, s_j, h_j, n_pj) <- acc
          , n_pi == n_pj
          , let !h_i = h_j <> h_si
          ]
     in go (n - 1) s_p''

-- | Compute ALBA parameters: Length of proof, seed number, and probability of selecting last tuple.
--
-- See corollary 1 in ALBA paper, p. 11.
computeParams :: Params -> (Integer, Integer, Double)
computeParams Params{λ_rel, λ_sec, n_p, n_f} =
  (u, d, q)
 where
  e = exp 1

  loge :: Double
  loge = logBase 2 e

  u' =
    (fromIntegral λ_sec + logBase 2 (fromIntegral λ_rel) + 1 + logBase 2 loge)
      / logBase 2 (fromIntegral n_p / fromIntegral n_f)

  u = ceiling u'

  d = ceiling $ (u' + log u') * fromIntegral λ_rel / loge

  q :: Double
  q = 2 * fromIntegral λ_rel / (fromIntegral d * loge)

modBS :: ByteString -> Integer -> Integer
modBS bs q =
  let n = fromBytes bs q
   in n `mod` q

fromBytes :: ByteString -> Integer -> Integer
fromBytes bs q = BS.foldl' (\acc b -> (acc * 256 + fromIntegral b) `mod` q) 0 bs

fromBytesLE :: ByteString -> Word64
fromBytesLE = either error id . runGet getWord64le . BS.take 8

toBytesLE :: Word64 -> ByteString
toBytesLE = runPut . putWord64le

writeProof :: FilePath -> Proof -> IO Int
writeProof file proof = do
  let serialized = encode proof
  BS.writeFile file serialized
  pure $ BS.length serialized

readProof :: FilePath -> IO Proof
readProof file = do
  serialized <- BS.readFile file
  pure $ either error id $ decode serialized

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
  k' = 1 .<<. k
  k = logBase2 (n * εFail)
  d = k' `div` n
  i = modPowerOf2 bytes k'

εFail :: Word64
εFail = 1 .<<. 40 -- roughly 1 in 10 billions

logBase2 :: FiniteBits b => b -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

modPowerOf2 :: ByteString -> Word64 -> Word64
modPowerOf2 bytes n =
  let r = fromBytesLE bytes
   in (n - 1) .&. r

isPowerOf2 :: Word64 -> Bool
isPowerOf2 n =
  countLeadingZeros n + countTrailingZeros n == 63

-- | Verify `Proof` that the set of elements known to the prover `s_p` has size greater than $n_f$.
verify :: Params -> Proof -> Bool
verify params@Params{n_p} (Proof (d, bs)) =
  let (u, _, q) = computeParams params

      check item = \case
        (True, _, []) ->
          let h = hash item
              l = oracle h n_p
              h_0 = hash d <> h
              n_p0 = h_0 `oracle` n_p
           in (l == n_p0, h_0, [item])
        (True, h_j, acc) ->
          let prf = item : acc
              h_si = hash item
              n_pi = h_si `oracle` n_p
              h_i = h_j <> h_si
              n_pj = oracle h_j n_p
              -- last round
              prob = ceiling $ 1 / q
              n_last = oracle h_i prob
              m =
                if length prf < length bs
                  then n_pj == n_pi
                  else n_last == 0
           in (m, h_i, item : acc)
        other -> other

      fst3 (a, _, _) = a
   in length bs == fromInteger u
        && fst3 (foldr check (True, Hash "", []) bs)
