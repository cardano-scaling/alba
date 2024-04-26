{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module ALBASpec where

import ALBA (
    Bytes (..),
    Hashable (..),
    Params (..),
    Proof (..),
    computeParams,
    fromBytes,
    fromBytesLE,
    isPowerOf2,
    modBS,
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
import Data.Serialize (encode)
import Debug.Trace
import Test.Hspec (Spec, SpecWith, describe, it)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (
    Gen,
    Large (..),
    Positive (..),
    Property,
    Small (..),
    arbitrary,
    choose,
    counterexample,
    cover,
    forAll,
    forAllShrink,
    frequency,
    label,
    property,
    resize,
    sized,
    vectorOf,
    (=/=),
    (===),
    (==>),
 )
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Property (conjoin)

spec :: Spec
spec = do
    prop "can hash bytestrings" prop_hashBytestring
    prop "can take naive modulus of a bytestring" prop_modBytestring
    modifyMaxSuccess (const 1000) $
        prop "can flip a single bit from a bytestring" prop_flip1Bit
    modifyMaxSuccess (const 1000) $
        prop "convert Integer to/from LE ByteString" prop_roundtripBytesInteger
    prop "asserts power-of-2-ness" prop_isPowerOf2
    modifyMaxSuccess (const 1000) $
        prop "can generate uniform random number within bound" prop_randomOracle
    describe "parameters w/in expected bounds" $
        mapM_
            checkParameters
            [ (Params 128 128 600 400, 232)
            , (Params 128 128 660 330, 136)
            , (Params 128 128 800 200, 68)
            ]

    prop "can verify small proof is valid" $ prop_verifyValidProof 100
    modifyMaxSuccess (const 10) $
        prop "can verify large proof is valid" $
            prop_verifyValidProof 300
    prop "can reject proof if items are tampered with" prop_rejectTamperedProof

prop_rejectTamperedProof :: Property
prop_rejectTamperedProof =
    forAll genModifiedProof $ \(original, tampered) -> do
        let params = Params 8 8 8 2
            (u, _, q) = computeParams params
        conjoin
            [ original =/= tampered
            , verify params tampered === False
            ]
            & counterexample ("u = " <> show u <> ", q = " <> show q)

genModifiedProof :: Gen (Proof, Proof)
genModifiedProof = do
    items <- resize 100 genItems
    let params = Params 8 8 80 20
        (u, _, q) = computeParams params
        proof@(Proof (n, bs)) = prove params items
    frequency
        [ (1, pure $ (proof, Proof (n + 1, bs)))
        , (length items, ((proof,) . (Proof . (n,))) <$> flip1Bit bs)
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

prop_verifyValidProof :: Integer -> Property
prop_verifyValidProof coeff =
    forAll (resize (fromInteger coeff) genItems) $ \items -> do
        let params = Params 8 8 (coeff * 8 `div` 10) (coeff * 2 `div` 10)
            (u, _, q) = computeParams params
            proof = prove params items
        verify params proof === True
            & counterexample ("u = " <> show u <> ", q = " <> show q <> ", proof = " <> show proof)

shrinkPowerOf2 :: Integer -> [Integer]
shrinkPowerOf2 n
    | n > 2 = [n `div` 2]
    | otherwise = []

genItems :: Gen [Bytes]
genItems = sized $ \n -> vectorOf n (Bytes . BS.pack <$> vectorOf 8 arbitrary)

checkParameters :: (Params, Integer) -> SpecWith ()
checkParameters (params, expected) =
    it ("check u = " <> show expected <> " for " <> show params) $
        let (u, _, _) = computeParams params
         in abs (u - expected) <= 3

prop_hashBytestring :: ByteString -> ByteString -> Property
prop_hashBytestring bytes1 bytes2 =
    bytes1 /= bytes2 ==> hash bytes1 =/= hash bytes2

prop_modBytestring :: Positive Integer -> Positive Integer -> Property
prop_modBytestring (Positive x) (Positive y) =
    y < x ==> modBS (toBytesLE x) y === x `mod` y

prop_randomOracle :: ByteString -> Property
prop_randomOracle bytes =
    forAll arbitrary $ \(Positive n) ->
        let bytesInteger = fromBytesLE bytes
            o = oracle bytes n
            oracleBytes = BS.dropWhile (== 0) $ toBytesLE o
            allButOneBytes = BS.reverse $ BS.drop 1 oracleBytes
         in bytesInteger
                > n
                ==> and (BS.zipWith (==) bytes allButOneBytes)
                    & counterexample ("fast oracle (as bytes): " <> show (BS.unpack oracleBytes))
                    & counterexample ("fast oracle: " <> show o)
                    & counterexample ("input (as integer): " <> show bytesInteger)
                    & counterexample ("input (as bytes):" <> show (BS.unpack bytes))

prop_roundtripBytesInteger :: Positive Integer -> Property
prop_roundtripBytesInteger (Positive n) =
    n >= 0 ==> fromBytesLE (toBytesLE n) === n

prop_isPowerOf2 :: Property
prop_isPowerOf2 =
    forAll genPowerOf2 $ \n ->
        isPowerOf2 n

genPowerOf2 :: Gen Integer
genPowerOf2 =
    arbitrary >>= \(Positive (Small k)) -> pure $ 2 ^ k
