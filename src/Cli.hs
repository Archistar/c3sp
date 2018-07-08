module Cli(cli) where

import CmdParser (Task(..), parse)
import System.Environment (getArgs)
import ConfigurationHandler (stdinValidateQuery, findConfigs, stdinFindConfigs)
import Strings (helpText)
import TestSuite (runAllTests)
import Data.Aeson
import System.Exit (die)
import qualified Data.ByteString.Lazy.Char8 as Ch8

printJSON :: (ToJSON a) => a -> IO ()
printJSON = Ch8.putStrLn . encode

performTask :: Task -> IO ()
performTask (FindConfigs path) = findConfigs path >>= either die printJSON
performTask StdinFindConfigs   = stdinFindConfigs >>= either die printJSON
performTask ShowHelp           = putStrLn helpText
performTask RunTests           = runAllTests >>= putStrLn
performTask RunDemo            = performTask $ FindConfigs "demo.json"
performTask ValidateQuery      =
  let printOK = putStrLn "Valid Query."
      exitErr = die . ("Invalid Query : " ++)
  in stdinValidateQuery >>= maybe printOK exitErr

-- Perform command specified by command line
cli :: IO ()
cli =
  let printHelp = putStrLn "Wrong usage. Use --help for help."
  in getArgs >>= maybe printHelp performTask . parse
