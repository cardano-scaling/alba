{-# LANGUAGE NamedFieldPuns #-}

import ALBA (Params (..), genItems, prove, writeProof)
import Data.Word (Word64)
import System.Environment (getArgs)
import Test.QuickCheck (generate, resize)

data Options = Options
  { size :: Word64
  , len :: Int
  , params :: Params
  , output :: FilePath
  }
  deriving (Show)

defaultOptions =
  Options
    { size = 100
    , len = 8
    , params = Params 8 8 80 20
    , output = "proof.alba"
    }

main :: IO ()
main = do
  opts@Options{size, len, params = pars@Params{n_p, n_f}, output} <- getArgs >>= parseOptions
  bs <- generate $ resize (fromIntegral size) $ genItems len
  let opts'@Options{params = params'} = opts{params = pars{n_p = size * n_p `div` 100, n_f = size * n_f `div` 100}}
  putStrLn $ "Generating proof " <> show opts'
  let prf = prove params' bs
  writeProof output prf >>= \n ->
    putStrLn ("Written proof to '" <> output <> "' (" <> show n <> " bytes)")

parseOptions :: [String] -> IO Options
parseOptions [] = pure defaultOptions
parseOptions ("--security" : lam : rest) = do
  let λ = read lam
  opts <- parseOptions rest
  pure $ opts{params = (params opts){λ_sec = λ, λ_rel = λ}}
parseOptions ("--size" : sz : rest) = do
  let size = read sz
  opts <- parseOptions rest
  pure $ opts{size}
parseOptions ("--len" : ln : rest) = do
  let len = read ln
  opts <- parseOptions rest
  pure $ opts{len}
parseOptions ("--honest-ratio" : hn : rest) = do
  let rat = read hn
  opts <- parseOptions rest
  pure $ opts{params = (params opts){n_p = rat, n_f = 100 - rat}}
parseOptions other = do
  error ("Invalid arguments: " <> unwords other)
