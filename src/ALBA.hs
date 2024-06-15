{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Redundant do" -}

module ALBA (
  -- * API

  -- ** Core Functions
  prove,
  verify,

  -- ** Utilities
  computeParams,
  genItems,
  oracle,
  writeProof,
  readProof,
  fromBytesLE,
  toBytesLE,
  modPowerOf2,
  isPowerOf2,

  -- * Types
  Bytes (..),
  Params (..),
  Proof (..),
  NoProof (..),
  Hashable (..),
  Retries (..),
  Verification (..),
)
where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Control.Monad.ST.Strict (ST, runST)
import Control.Monad.State.Strict (State, evalState)
import Data.Bits (FiniteBits, countLeadingZeros, finiteBitSize, (.&.), (.<<.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Data.ByteString.Internal (unsafeCreate)
import Data.STRef (newSTRef)
import Data.STRef.Strict (STRef)
import Data.Serialize (Serialize (..), decode, encode, getWord64le, putWord64le, runGet, runPut)
import Data.String (IsString (..))
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
  { λ_sec :: !Integer
  -- ^ Security parameter
  -- Controls the probability that `extract` returns a set of size less than `n_f`.
  -- 128 seems like a good value
  , λ_rel :: !Integer
  -- ^ Verification parameter.
  -- Controls the probability that `verify` returns `True` when the proof is invalid.
  -- 128 seems like a good value
  , n_p :: !Word64
  -- ^ Estimated size of "honest" parties set.
  , n_f :: !Word64
  -- ^ Estimated size of "adversarial" parties set.
  }
  deriving (Show, Eq)

data Proof = Proof
  { index :: !Integer
  -- ^ The initial index with which the proof was generated
  , retryCount :: !Integer
  -- ^ Number of retries before a proof was generated
  , elements :: ![Bytes]
  -- ^ The set of elements witnessing knowledge of a set of size greater than $n_f$.
  }
  deriving (Show, Eq)

instance Serialize Proof where
  put Proof{index, retryCount, elements} = do
    put index
    put retryCount
    put elements

  get = do
    index <- get
    retryCount <- get
    elements <- get
    pure $ Proof index retryCount elements

newtype NoProof = NoProof Retries
  deriving stock (Show, Eq)

newtype Retries = Retries Integer
  deriving stock (Show, Eq)

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

instance IsString Bytes where
  fromString = Bytes . Hex.decodeLenient . fromString

genItems :: Int -> Gen [Bytes]
genItems len = sized $ \n -> vectorOf n (Bytes . BS.pack <$> vectorOf len arbitrary)

data ProofStep = ProofStep
  { t :: !Integer
  , s :: ![Bytes]
  , h :: !Hash
  , n :: !Word64
  }
  deriving (Show, Eq)

-- | Output a proof `the set of elements known to the prover `s_p` has size greater than $n_f$.
--
-- This version of `prove` is much more efficient than the original
-- one as constructs the proof using depth-first search over the
-- required length.
prove :: Params -> [Bytes] -> Either NoProof Proof
prove params@Params{λ_sec, n_p} s_p =
  runST $ newSTRef 0 >>= proveWithRetry 0
 where
  hashBounds = λ_sec * λ_sec

  (u, d, q) = computeParams params

  prob_q = ceiling $ 1 / q

  proveWithRetry :: Integer -> STRef s Int -> ST s (Either NoProof Proof)
  proveWithRetry retryCount hashCount
    | retryCount >= λ_sec = pure $ Left $ NoProof $ Retries retryCount
    | otherwise = do
        step0 <- round0 preHash hashCount
        stepn <- start hashCount preHash step0
        case stepn of
          Nothing -> proveWithRetry (retryCount + 1) hashCount
          Just prf -> pure $ Right prf{retryCount}
   where
    preHash = zip s_p $ map (\bs -> let h = hash retryCount <> hash bs in (h, h `oracle` n_p)) s_p

  round0 :: [(Bytes, (Hash, Word64))] -> STRef s Int -> ST s [ProofStep]
  round0 preHash hashCount =
    pure
      [ ProofStep t [s_i] h_0 n_p0
      | (s_i, (h, l)) <- preHash
      , t <- [1 .. d]
      , let !h_0 = hash t <> h
      , let n_p0 = h_0 `oracle` n_p
      , l == n_p0
      ]

  start :: STRef s Int -> [(Bytes, (Hash, Word64))] -> [ProofStep] -> ST s (Maybe Proof)
  start hashCount preHash = \case
    [] -> pure Nothing
    (element : elements) ->
      case go (fromInteger $ u - 2) preHash element of
        Nothing -> start hashCount preHash elements
        prf -> pure prf
   where
    go :: Int -> [(Bytes, (Hash, Word64))] -> ProofStep -> Maybe Proof
    go 0 ((s_i, (h_si, _)) : rest) step@(ProofStep n acc h_j n_pj) =
      let h_i = h_j <> h_si
          n_pj' = h_i `oracle` prob_q
       in if n_pj' == 0
            then Just $ Proof n 0 (s_i : acc)
            else go 0 rest step
    go _ [] _ = Nothing
    go k ((s_i, (h_si, n_pi)) : rest) step@(ProofStep n acc h_j n_pj) =
      let h_i = h_j <> h_si
       in if n_pi == n_pj
            then case go (k - 1) preHash (ProofStep n (s_i : acc) h_i (h_i `oracle` n_p)) of
              Nothing -> go k rest step
              prf -> prf
            else go k rest step

-- | Compute ALBA parameters: Length of proof, seed number, and probability of selecting last tuple.
--
-- See corollary 2 in ALBA paper, p. 19.
computeParams :: Params -> (Integer, Integer, Double)
computeParams Params{λ_rel, λ_sec, n_p, n_f} =
  (u, d, q)
 where
  e = exp 1

  log_2 = logBase 2

  loge :: Double
  loge = log_2 e

  log3 :: Double
  log3 = log_2 3

  u' =
    (fromIntegral λ_sec + log_2 (fromIntegral λ_rel + log3) + 1 - log_2 loge)
      / logBase 2 (fromIntegral n_p / fromIntegral n_f)

  u = ceiling u'

  d = ceiling $ 16 * u' * (fromIntegral λ_rel + log3) / loge

  q :: Double
  q = 2 * (fromIntegral λ_rel + log3) / (fromIntegral d * loge)

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

data Verification
  = Verified {proof :: !Proof, params :: !Params}
  | InvalidItem {proof :: !Proof, item :: !Bytes, level :: !Int}
  | InvalidLength {proof :: !Proof, expected :: !Int}
  deriving (Show, Eq)

-- | Verify `Proof` that the set of elements known to the prover `s_p` has size greater than $n_f$.
verify :: Params -> Proof -> Verification
verify params@Params{n_p} proof@(Proof d k bs) =
  let (u, _, q) = computeParams params

      check item = \case
        (v@Verified{}, _, []) ->
          let h = hash k <> hash item
              l = oracle h n_p
              h_0 = hash d <> h
              n_p0 = h_0 `oracle` n_p
              m = if l == n_p0 then v else InvalidItem{proof, item, level = 0}
           in (m, h_0, [item])
        (v@Verified{}, h_j, acc) ->
          let prf = item : acc
              h_si = hash k <> hash item
              n_pi = h_si `oracle` n_p
              h_i = h_j <> h_si
              n_pj = oracle h_j n_p
              -- last round
              prob = ceiling $ 1 / q
              n_last = oracle h_i prob
              m
                | length prf < length bs && n_pj == n_pi = v
                | length prf == length bs && n_last == 0 = v
                | otherwise = InvalidItem{proof, item, level = length prf}
           in (m, h_i, item : acc)
        other -> other

      fst3 (a, _, _) = a
   in if length bs /= fromInteger u
        then InvalidLength{proof, expected = fromInteger u}
        else fst3 (foldr check (Verified{proof, params}, Hash "", []) bs)
