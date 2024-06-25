import ALBA (Bytes (..), Params (..), Proof, hash, prove, verify)
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad (forM)
import Criterion (Benchmark, env, perRunEnv)
import Criterion.Main (bench, bgroup, defaultMain, nf, whnf)
import Criterion.Types (Benchmarkable (perRun))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.QuickCheck (Gen, arbitrary, generate, vectorOf)

main :: IO ()
main = do
  let benchSizes = [1000, 5000, 10000]
  defaultMain
    [ bgroup
        "Proving"
        $ [ benchProof (b, s_p, n_p)
          | b <- benchSizes
          , n_p <- [60, 66, 80]
          , let high = fromIntegral b
          , let low = high * n_p `div` 100
          , let mid = (high + low) `div` 2
          , s_p <- [low, mid, high]
          ]
    , bgroup
        "Verifying"
        [ benchVerification (b, n_p)
        | b <- benchSizes
        , n_p <- [60, 66, 80]
        ]
    ]

benchVerification :: (Int, Int) -> Benchmark
benchVerification (total, n_p) =
  let params = Params 128 128 (fromIntegral $ total * n_p `div` 100) (fromIntegral $ (total * (100 - n_p)) `div` 100)
      label = "s_p = " <> show total <> "/n_p = " <> show n_p <> "%"
      mkProof bytes = either (error . show) id $ prove params (Bytes <$> bytes)
   in -- NOTE: no need to generate different proofs per run as the verification time is
      -- tied to the proof size, not the proof content
      env (mkProof <$> genItems (total, 710)) $ \proof ->
        bench label $
          nf (verify params) proof

benchHash :: ByteString -> Benchmark
benchHash bytes =
  bench label $ nf hash bytes
 where
  label = "hashing len=" <> show (BS.length bytes)

benchProof :: (Int, Int, Int) -> Benchmark
benchProof (total, s_p, n_p) =
  let params = Params 128 128 (fromIntegral $ total * n_p `div` 100) (fromIntegral $ (total * (100 - n_p)) `div` 100)
      label = "total = " <> show total <> "/s_p = " <> show s_p <> "/n_p = " <> show n_p <> "%"
   in bench label $
        -- NOTE: we generate a new proof per run to highlight the potential high variance in proving time
        -- depending on the bytes generated
        perRunEnv (genItems (s_p, 710)) $ \bytes ->
          evaluate $ rnf $ prove params (Bytes <$> bytes)

genItems :: (Int, Int) -> IO [ByteString]
genItems (numItems, itemSize) =
  generate $ vectorOf numItems (BS.pack <$> vectorOf itemSize (arbitrary :: Gen Word8))
