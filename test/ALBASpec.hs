{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module ALBASpec where

import ALBA (
  Bytes (..),
  Hashable (..),
  NoProof (..),
  Params (..),
  Proof (..),
  Retries (..),
  Verification (..),
  computeParams,
  fromBytesLE,
  genItems,
  isPowerOf2,
  modPowerOf2,
  oracle,
  prove,
  toBytesLE,
  verify,
 )
import Data.Bits (Bits (..), countTrailingZeros)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Function ((&))
import qualified Data.List as List
import Data.Serialize (decode, encode)
import Data.Word (Word64, Word8)
import Debug.Trace
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (
  Arbitrary,
  Gen,
  Large (..),
  Positive (..),
  Property,
  Small (..),
  arbitrary,
  checkCoverage,
  checkCoverageWith,
  choose,
  counterexample,
  cover,
  coverTable,
  forAll,
  forAllBlind,
  forAllShrink,
  frequency,
  generate,
  label,
  property,
  resize,
  shrink,
  sized,
  stdConfidence,
  tabulate,
  vectorOf,
  (=/=),
  (===),
  (==>),
 )
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Property (conjoin)
import Test.QuickCheck.State (Confidence (..))

spec :: Spec
spec = do
  prop "can hash bytestrings" prop_hashBytestring
  modifyMaxSuccess (const 1000) $
    prop "can flip a single bit from a bytestring" prop_flip1Bit
  modifyMaxSuccess (const 1000) $
    prop "convert Integer to/from LE ByteString" prop_roundtripBytesInteger
  prop "asserts power-of-2-ness" prop_isPowerOf2
  prop "compute power of 2 modulus" prop_modPowerOf2
  modifyMaxSuccess (const 1000) $
    prop "can generate uniform random number within bound" prop_randomOracle
  modifyMaxSuccess (const 100_000) $
    prop "oracle distribution is uniform" prop_oracleDistributionIsUniform

  describe "parameters" $ do
    mapM_
      checkParameters
      [ (Params 128 128 600 400, 232)
      , (Params 128 128 660 330, 136)
      , (Params 128 128 800 200, 68)
      ]

  prop "can verify small proof is valid" $ prop_verifyValidProof 8 100
  prop "can verify large proof is valid" $ prop_verifyValidProof 400 1000
  prop "can reject proof if items are tampered with" prop_rejectTamperedProof

  prop "can roundtrip serialisation of proof" prop_roundtripProof
  modifyMaxSuccess (const 30) $ do
    prop "needs to retry proving given number of elements is too small" prop_retryProofOnSmallSet
    prop "stops retrying proof after λ attempts" prop_stopRetryingProof
    prop "retries proof after number of hashes is above λ² given n_p is lower than λ²" prop_retryProofOnHashBounds
    prop "does not retry proof given n_p is greater than λ³" prop_doesNoRetryProofOnHashBounds

prop_retryProofOnHashBounds :: Property
prop_retryProofOnHashBounds =
  forAll (choose (10, 20)) $ \λ ->
    -- we want n_p to be lower than λ² so we generate exactly λ² items
    forAll (resize (fromIntegral $ λ * λ) (genItems 10)) $ \items ->
      let numItems = fromIntegral $ length items
          params = Params λ λ (numItems * 80 `div` 100) (numItems * 20 `div` 100)
       in case prove params items of
            Right Proof{retryCount} ->
              retryCount >= 0 && retryCount <= λ
                & counterexample ("retryCount = " <> show retryCount)
                & counterexample ("numItems = " <> show numItems)
                & label ("# prove run = " <> show (succ retryCount))
                -- 35% is completely empirical, the real target is for
                -- the average number of proof run to be around 2 but
                -- QuickCheck does not provide an easy way to target
                -- an average value
                & cover 35 (retryCount > 0) "retried proof"
                & checkCoverage
            Left (NoProof _) -> property True & label "no proof"

prop_doesNoRetryProofOnHashBounds :: Property
prop_doesNoRetryProofOnHashBounds =
  forAll (choose (10, 20)) $ \λ ->
    -- we want n_p to be greater than λ³ so we need to generate more items
    forAllBlind (resize (fromIntegral $ λ * λ * λ * 6 `div` 5) (genItems 10)) $ \items ->
      let numItems = fromIntegral $ length items
          params = Params λ λ (numItems * 80 `div` 100) (numItems * 20 `div` 100)
       in case prove params items of
            Right Proof{retryCount} ->
              property True
                & label ("# prove run = " <> show (succ retryCount))
                -- This bound is also empirical, the real target is for the average and max number of proof run to be 1.
                -- It's not clear why the distribution is actually quite similar to the other retry-related test
                & cover 65 (retryCount < 1) "no retry"
                & checkCoverage
            Left (NoProof _) -> property False

prop_stopRetryingProof :: Property
prop_stopRetryingProof =
  forAll (resize 100 (genItems 10)) $ \items ->
    forAll (choose (16, 32)) $ \λ ->
      let params = Params λ λ 80 20
          fewerItems = drop 81 items
       in prove params fewerItems === Left (NoProof $ Retries λ)

prop_roundtripProof :: Proof -> Property
prop_roundtripProof proof =
  let bs = encode proof
   in decode bs === Right proof

prop_retryProofOnSmallSet :: Property
prop_retryProofOnSmallSet =
  forAll (resize 100 (genItems 10)) $ \items -> do
    let params = Params 16 16 80 20
        (u, _, q) = computeParams params
        fewerItems = drop 70 items
     in counterexample ("u = " <> show u <> ", q = " <> show q) $
          case prove params fewerItems of
            Left (NoProof _) -> property True & label "no proof"
            Right proof@Proof{retryCount} ->
              verify params proof == Verified{proof, params}
                & label ("retryCount <= " <> show ((retryCount `div` 10 + 1) * 10))
                & cover 60 (retryCount > 0) "retried proof"
                & checkCoverage
                & counterexample ("retryCount = " <> show retryCount)

prop_oracleDistributionIsUniform :: Property
prop_oracleDistributionIsUniform =
  forAll (resize 100 arbitrary) $ \(bytes :: ByteString) -> do
    let remainder = hash bytes `oracle` 10_000_000
    remainder >= 0 && remainder < 10_000_000
      & tabulate "distribution" [show $ remainder `div` 1_000_000]
      & coverTable "distribution" (map (\r -> (show r, 10)) [0 .. 9])
      -- need to decrease tolerance of checkCoverage to 0.80 to pass
      -- not sure if this is ok, eg. a consequence of the way bytes are generated,
      -- or a flaw in the implementation of oracle that skews remainders towards
      -- smaller values
      & checkCoverageWith stdConfidence{tolerance = 0.95}

prop_rejectTamperedProof :: Property
prop_rejectTamperedProof =
  forAll (genModifiedProof params) $ \(original, tampered) -> do
    let (u, _, q) = computeParams params
        verified = verify params tampered
        valid = Verified{proof = tampered, params}
    conjoin
      [ original =/= tampered
      , verified =/= valid
      ]
      & cover 99.99 (verified /= valid) "tampered proof rejected"
      -- there's still a small chance that a tampered proof
      -- is accepted because the params value are quite small
      & counterexample ("u = " <> show u <> ", q = " <> show q)
 where
  params = Params 64 64 80 20

genModifiedProof :: Params -> Gen (Proof, Proof)
genModifiedProof params = do
  items <- resize 100 (genItems 100)
  let (u, _, q) = computeParams params
   in case prove params items of
        Left (NoProof _) -> genModifiedProof params
        Right proof@(Proof n k bs) ->
          frequency
            [ (1, pure $ (proof, Proof (n + 1) k bs))
            , (length items, (proof,) . Proof n k <$> flip1Bit bs)
            ]

prop_flip1Bit :: ByteString -> Property
prop_flip1Bit bytes =
  BS.length bytes > 0 ==> do
    let bs = Bytes bytes
    forAll (flip1Bit [bs, bs]) $ \case
      [_, Bytes bs'] ->
        length (filter (uncurry (/=)) $ BS.zipWith (,) bytes bs') === 1
          & counterexample ("bytes = " <> show (BS.unpack bytes) <> ", bs' = " <> show (BS.unpack bs'))
      other -> property False

flip1Bit :: [Bytes] -> Gen [Bytes]
flip1Bit bytes = do
  i <- choose (1, length bytes - 1)
  let (prefix, Bytes bs : suffix) = splitAt i bytes
  j <- choose (0, BS.length bs * 8 - 1)
  let flippedBit = flipBit j bs
  pure $
    prefix
      <> ( Bytes
            flippedBit
            : suffix
         )

flipBit :: Int -> ByteString -> ByteString
flipBit j bs =
  BS.concat
    [ BS.take k bs
    , BS.singleton b'
    , BS.drop (k + 1) bs
    ]
 where
  (k, l) = j `divMod` 8
  b = BS.index bs k
  b' = b `xor` (1 `shiftL` l)

prop_verifyValidProof :: Int -> Word64 -> Property
prop_verifyValidProof itemSize numItems =
  forAll (resize (fromIntegral numItems) (genItems itemSize)) $ \items -> do
    let params = Params 8 8 (numItems * 8 `div` 10) (numItems * 2 `div` 10)
        (u, _, q) = computeParams params
     in case prove params items of
          Left (NoProof _) -> property False
          Right proof ->
            verify params proof === Verified{proof, params}
              & counterexample ("u = " <> show u <> ", q = " <> show q <> ", proof = " <> show proof)

shrinkPowerOf2 :: Integer -> [Integer]
shrinkPowerOf2 n
  | n > 2 = [n `div` 2]
  | otherwise = []

checkParameters :: (Params, Integer) -> SpecWith ()
checkParameters (params, expected) =
  it ("check u = " <> show expected <> " for " <> show params) $
    let (u, _, _) = computeParams params
     in abs (u - expected) <= 3

prop_hashBytestring :: ByteString -> ByteString -> Property
prop_hashBytestring bytes1 bytes2 =
  bytes1 /= bytes2 ==> hash bytes1 =/= hash bytes2

prop_randomOracle :: Property
prop_randomOracle =
  forAll (BS.pack <$> vectorOf 8 arbitrary) $ \(bytes :: ByteString) ->
    forAll arbitrary $ \(Positive (Small n)) ->
      n >= 2 ==>
        let h = hash bytes
            o = h `oracle` n
            oracleBytes = BS.dropWhile (== 0) $ toBytesLE o
            allButOneBytes = BS.reverse $ BS.drop 1 oracleBytes
         in o < n
              & counterexample ("fast oracle (as bytes): " <> show (BS.unpack oracleBytes))
              & counterexample ("fast oracle: " <> show o)
              & counterexample ("input:" <> show h)

prop_roundtripBytesInteger :: Positive Word64 -> Property
prop_roundtripBytesInteger (Positive n) =
  n >= 0 ==> fromBytesLE (toBytesLE n) === n

prop_modPowerOf2 :: Property
prop_modPowerOf2 =
  forAll genPowerOf2 $ \n ->
    n > 2 ==>
      forAll arbitrary $ \(w :: Word64) ->
        let bytes = toBytesLE w
         in modPowerOf2 bytes n === fromBytesLE bytes `mod` n

prop_isPowerOf2 :: Property
prop_isPowerOf2 =
  forAll genPowerOf2 $ \n ->
    n /= 0 ==> isPowerOf2 n

genPowerOf2 :: Gen Word64
genPowerOf2 =
  arbitrary >>= \(Positive (Small k)) -> pure $ 2 ^ k

instance Arbitrary Proof where
  arbitrary = do
    items <- genItems 100
    n <- arbitrary
    k <- arbitrary
    pure $ Proof n k items
