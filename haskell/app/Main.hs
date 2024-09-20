{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import ALBA (Bytes (..), NoProof (..), Params (..), Proof (..), Verification (Verified), genItems, prove, readProof, verify, writeProof)
import Control.Monad (forM, forM_, unless)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath ((</>))
import Test.QuickCheck (generate, resize)

data Command
  = Prove !Options
  | Verify !Options
  | Generate !Options

data Options = Options
  { size :: Word64
  , len :: Int
  , params :: Params
  , input :: Maybe FilePath
  , output :: FilePath
  }
  deriving (Show)

defaultOptions =
  Options
    { size = 100
    , len = 8
    , params = Params 128 128 80 20
    , input = Nothing
    , output = "proof.alba"
    }

main :: IO ()
main = do
  opts <- getArgs >>= parseCommand
  case opts of
    Prove opts@Options{size, len, params, output, input} -> do
      bs <- case input of
        Just dir -> do
          putStrLn $ "Reading items from " <> dir
          forM [1 .. size] $ \idx ->
            Bytes <$> BS.readFile (dir </> show idx)
        Nothing -> do
          putStrLn "Generating random items"
          generate $ resize (fromIntegral size) $ genItems len
      putStrLn $ "Generating proof " <> show opts
      case prove params bs of
        Left (NoProof retries) ->
          putStrLn ("No proof could be written after " <> show retries <> " retries") >> exitWith (ExitFailure 1)
        Right prf@Proof{retryCount} ->
          writeProof output prf >>= \n ->
            putStrLn ("Written proof to '" <> output <> "' (" <> show n <> " bytes, " <> show retryCount <> " retries)")
    Verify opts@Options{size, len, params, output} -> do
      putStrLn $ "Verifying proof with " <> show opts
      readProof output >>= \prf ->
        case verify params prf of
          Verified{} -> putStrLn ("Verified proof " <> show prf)
          other -> putStrLn ("Cannot verify proof " <> show prf <> ", failure: " <> show other) >> exitWith (ExitFailure 1)
    Generate opts@Options{output} -> do
      let baseDir = output
      putStrLn $ "Generating random items to " <> baseDir
      exist <- doesDirectoryExist baseDir
      unless exist $ createDirectory baseDir
      bs <- generate $ resize (fromIntegral $ size opts) $ genItems (len opts)
      forM_ (zip bs [1 ..]) $
        \(Bytes bytes, idx) -> BS.writeFile (baseDir </> show idx) bytes
      putStrLn $ "Generated " <> show (length bs) <> " items"

usage :: IO ()
usage =
  putStrLn $
    unlines
      [ "alba: Command-line utility for creating and verifying ALBA proofs"
      , ""
      , "Usage:"
      , "alba prove <options>    : Generate an ALBA proof file from a (random) set of items"
      , "alba verify <options>   : Verify an ALBA proof. Note that options must be consistent with"
      , "alba generate <options> : Generate a set of random items to be used for proof generation"
      , ""
      , "Options:"
      , "--help           : Display this help text"
      , "--security <int> : The security level of the proof (default: 128)"
      , "--size <int>     : The actual number of elements to build a proof for (default: 100)"
      , "--n-p <int>      : ALBA n_p parameter, e.g expected \"honest\" set size (default: 80)"
      , "--n-f <int>      : ALBA n_f parameter, e.g expected \"faulty\" set size (default: 20)"
      , "--len <int>      : The length (in bytes) of each item in the input set (default: 8)"
      , "--output <file>  : The file containing proof to write or verify, or the directory where to generate"
      , "                   items in (default: alba.proof)"
      , "--input <dir>    : If set, reads the item to prove from the given directory instead of generating"
      , "                   them"
      ]

parseCommand :: [String] -> IO Command
parseCommand = \case
  ("prove" : rest) ->
    Prove <$> parseOptions rest
  ("verify" : rest) ->
    Verify <$> parseOptions rest
  ("generate" : rest) ->
    Generate <$> parseOptions rest
  ("--help" : _) ->
    usage >> exitSuccess
  other ->
    usage >> exitWith (ExitFailure 1)

parseOptions = \case
  [] -> pure defaultOptions
  ("--help" : _) ->
    usage >> exitSuccess
  ("--security" : lam : rest) -> do
    let λ = read lam
    opts <- parseOptions rest
    pure $ opts{params = (params opts){λ_sec = λ, λ_rel = λ}}
  ("--size" : sz : rest) -> do
    let size = read sz
    opts <- parseOptions rest
    pure $ opts{size}
  ("--n-p" : np : rest) -> do
    let n_p = read np
    opts <- parseOptions rest
    pure $ opts{params = (params opts){n_p}}
  ("--n-f" : nf : rest) -> do
    let n_f = read nf
    opts <- parseOptions rest
    pure $ opts{params = (params opts){n_f}}
  ("--len" : ln : rest) -> do
    let len = read ln
    opts <- parseOptions rest
    pure $ opts{len}
  ("--output" : output : rest) -> do
    opts <- parseOptions rest
    pure $ opts{output}
  ("--input" : input : rest) -> do
    opts <- parseOptions rest
    pure $ opts{input = Just input}
  other -> do
    usage >> exitWith (ExitFailure 2)
