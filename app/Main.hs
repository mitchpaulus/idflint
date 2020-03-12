module Main where

import System.Environment
import Lib

main :: IO ()
main = do
    args <- getArgs
    if parseArgs args then putStrLn "energyplus-lint\n\nUSAGE: ep-lint [options].." else return ()
    return ()

parseArgs :: [String] -> Bool
parseArgs args
        | any isHelp args = True
        | otherwise       = False

isHelp :: String -> Bool
isHelp argument = argument == "-h" || argument == "--help"


data EpLintOptions = EpLintOptions {
                                     help :: Bool,
                                     filepath :: String
                                   } deriving Show

defaultLintOptions = EpLintOptions { help = False, filepath = "in.idf" }
