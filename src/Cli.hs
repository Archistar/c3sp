module Cli where

import CmdParser (Task(..), parse)
import System.Environment (getArgs)
import ConfigurationHandler (stdinValidateQuery, findConfigs, stdinFindConfigs)
import Strings (helpText)
import TestSuite (runAllTests)
import Data.Aeson
import System.Exit (die)
import qualified Data.ByteString.Lazy.Char8 as Ch8

performTask :: Task -> IO ()
performTask (FindConfigs path) = findConfigs path >>= either die (Ch8.putStrLn . encode)
performTask StdinFindConfigs = stdinFindConfigs >>= either die (Ch8.putStrLn . encode)
performTask ShowHelp = putStrLn helpText
performTask RunTests = runAllTests >>= putStrLn
performTask RunDemo = performTask $ FindConfigs "demo.json"
performTask ValidateQuery = stdinValidateQuery >>= maybe (putStrLn "Valid Query.") (die . ("Invalid Query: " ++))

cli :: IO ()
cli =
  let printHelp = putStrLn "Wrong usage. Use --help for help."
  in getArgs >>= maybe printHelp performTask . parse
