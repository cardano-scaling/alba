import ALBA (Bytes (..), Params (..), Proof, hash, prove)
import Control.Monad (forM)
import Criterion (Benchmark)
import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.QuickCheck (Gen, arbitrary, generate, vectorOf)

main :: IO ()
main = do
  benches <-
    forM
      [ (s_p, len) | s_p <- [100, 200, 300], len <- [64, 128, 256]
      ]
      genItems
  defaultMain
    [ bgroup
        "Hashing"
        $ map benchHash (filter ((== 256) . BS.length . head) benches)
    , bgroup
        "Proof Generation"
        -- code does not currently run in a reasonable time for larger values of λ
        $ [benchProof (b, λ) | b <- benches, λ <- [8, 9, 10]]
    ]

benchHash :: [ByteString] -> Benchmark
benchHash bytes =
  bench label $ whnf hash bytes
 where
  label = "hashing " <> show (length bytes) <> " strings, len=" <> show (BS.length $ head bytes)

benchProof :: ([ByteString], Integer) -> Benchmark
benchProof (bytes, λ) =
  let coeff = fromIntegral $ length bytes
      params = Params λ λ (coeff * 8 `div` 10) (coeff * 2 `div` 10)
      label = "Proving " <> show coeff <> "/" <> show (BS.length $ head bytes) <> " (Params: " <> show params <> ")"
   in bench label $
        whnf
          (uncurry prove)
          (params, Bytes <$> bytes)

genItems :: (Int, Int) -> IO [ByteString]
genItems (numItems, itemSize) =
  generate $ vectorOf numItems (BS.pack <$> vectorOf itemSize (arbitrary :: Gen Word8))
