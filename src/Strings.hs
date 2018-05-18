module Strings where

format :: String -> String
format ('\t':xs) = tab ++ format xs
format [] = []
format (x:xs) = x:format xs

tab = "    "

helpText = format
    "Usage: Main OPTION\n\n\
    \Options:\n\t\
    \(-f | --find-configs) FILE   \tfind all possible server configurations by means of the json config file\n\t\
    \(-h | --help)                \tshow this help text\n\t\
    \(-t | --tests)               \trun tests\n\n\
    \File format:\n\t\
    \json"
