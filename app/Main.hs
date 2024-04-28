{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import ALBA (Params (..), genItems, prove, readProof, verify, writeProof)
import Data.Word (Word64)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import Test.QuickCheck (generate, resize)

data Command
  = Prove !Options
  | Verify !Options

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
  opts <- getArgs >>= parseCommand
  case opts of
    Prove opts@Options{size, len, params = pars@Params{n_p, n_f}, output} -> do
      bs <- generate $ resize (fromIntegral size) $ genItems len
      let opts'@Options{params} = adjustForSize opts
      putStrLn $ "Generating proof " <> show opts'
      let prf = prove params bs
      writeProof output prf >>= \n ->
        putStrLn ("Written proof to '" <> output <> "' (" <> show n <> " bytes)")
    Verify opts@Options{size, len, params = pars@Params{n_p, n_f}, output} -> do
      let opts'@Options{params} = adjustForSize opts
      putStrLn $ "Verifying proof with " <> show opts'
      readProof output >>= \prf ->
        if verify params prf
          then putStrLn ("Verified proof " <> show prf)
          else putStrLn ("Cannot verify proof " <> show prf) >> exitWith (ExitFailure 1)

adjustForSize :: Options -> Options
adjustForSize opts@Options{size, params = pars@Params{n_p, n_f}} =
  opts{params = pars{n_p = size * n_p `div` 100, n_f = size * n_f `div` 100}}

parseCommand :: [String] -> IO Command
parseCommand = \case
  ("prove" : rest) ->
    Prove <$> parseOptions rest
  ("verify" : rest) ->
    Verify <$> parseOptions rest
  other ->
    exitWith (ExitFailure 1)

parseOptions = \case
  [] -> pure defaultOptions
  ("--security" : lam : rest) -> do
    let λ = read lam
    opts <- parseOptions rest
    pure $ opts{params = (params opts){λ_sec = λ, λ_rel = λ}}
  ("--size" : sz : rest) -> do
    let size = read sz
    opts <- parseOptions rest
    pure $ opts{size}
  ("--len" : ln : rest) -> do
    let len = read ln
    opts <- parseOptions rest
    pure $ opts{len}
  ("--honest-ratio" : hn : rest) -> do
    let rat = read hn
    opts <- parseOptions rest
    pure $ opts{params = (params opts){n_p = rat, n_f = 100 - rat}}
  other -> do
    error ("Invalid arguments: " <> unwords other)
