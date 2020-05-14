module Main where

import System.Environment
import Lib
import ObjectTypes
import Data.Char

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


removeCommentLine :: String -> String
removeCommentLine line = takeWhile (/= '!') line

removeComments :: String -> String
removeComments idfFile = (unlines . (map removeCommentLine) . lines) idfFile


splitOnChar :: Char -> String -> [String]
splitOnChar char s = case dropWhile (== char) s of
                      "" -> []
                      s' -> w : splitOnChar char s''
                            where (w, s'') = break (== char) s'

splitObjects :: String -> [String]
splitObjects idfFile = splitOnChar ';' idfFile

splitFields :: String -> [String]
splitFields idfObject = splitOnChar ',' idfObject

lexIdf :: String -> [IdfObject]
lexIdf idfFile =  fmap splitFields (splitObjects (removeComments idfFile))

type IdfField  = String
type IdfObject = [IdfField]

data LexResult = LexResult { lexResultLineNum :: Int, lexResultMessage :: String }

-- This signals a malformed field or object and should be considered a lexical error.
newLineAfterField :: String -> Bool
newLineAfterField field = elem '\n' (dropWhile isSpace field)

badFields = filter newLineAfterField (concat (lexIdf testFile))

testFile = unlines
 [
    "Building,",
    "    701A,                    !- Name",
    "    0,                       !- North Axis {deg}",
    "    Suburbs,                 !- Terrain",
    "    0.04,                    !- Loads Convergence Tolerance Value",
    "    0.4,                    !- Temperature Convergence Tolerance Value {deltaC}",
    "    FullExterior,            !- Solar Distribution",
    "    25,                      !- Maximum Number of Warmup Days",
    "    6;                       !- Minimum Number of Warmup Days",
    "",
    "",
    "!-   ===========  ALL OBJECTS IN CLASS: TIMESTEP ===========",
    "",
    "Timestep,",
    "    4;                       !- Number of Timesteps per Hour"
 ]
