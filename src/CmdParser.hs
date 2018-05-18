module CmdParser(Task(..), parse) where

data Task = StdinFindConfigs
          | ValidateQuery
          | FindConfigs FilePath
          | RunDemo
          | RunTests
          | ShowHelp
          deriving (Show, Eq)

-- Parse a list of arguments and generate the corresponding Task.
parse :: [String] -> Maybe Task
parse [] = Just StdinFindConfigs
parse [arg1]
  | arg1 == "-h" || arg1 == "--help" = Just ShowHelp
  | arg1 == "-v" || arg1 == "--validate" = Just ValidateQuery
  | arg1 == "-t" || arg1 == "--tests" = Just RunTests
  | arg1 == "-d" || arg1 == "--demo" = Just RunDemo
  | otherwise = Nothing
parse (arg1:arg2:rest)
  | arg1 == "-f" || arg1 == "--find-configs" = Just $ FindConfigs arg2
  | otherwise = Nothing
