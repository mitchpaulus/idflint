{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import System.Directory
import Lib
import ObjectTypes
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    args <- getArgs
    let options = parseArgs args defaultLintOptions
    if help options then putStrLn "idf-lint\n\nUSAGE: ep-lint [options].."
    else if parseIdd options then putStrLn "Parsing idd..."
    else do
             let path = filepath options
             fileExists <- doesFileExist path
             if fileExists
             then do
                  idfFile <- TIO.readFile path
                  TIO.putStrLn $ lintIdf idfFile
                  --TIO.putStrLn idfFile
             else putStrLn $ path ++ "Not found."

parseArgs :: [String] -> EpLintOptions -> EpLintOptions
parseArgs (x:xs) options
            | x == "-h" || x == "--help" = parseArgs xs (options { help = True })
            | x == "-i" || x == "--idd"  = parseArgs xs (options { parseIdd = True })
            | otherwise                  = parseArgs xs (options { filepath = x })

parseArgs [] options = options

data EpLintOptions = EpLintOptions {
                                     help :: Bool,
                                     filepath :: String,
                                     parseIdd :: Bool
                                   } deriving Show

defaultLintOptions = EpLintOptions { help = False, filepath = "in.idf", parseIdd = False }


removeCommentLine :: T.Text -> T.Text
removeCommentLine = T.takeWhile (/= '!')

removeComments :: T.Text -> T.Text
removeComments = T.unlines . (map removeCommentLine) . T.lines


splitOnChar :: Char -> T.Text -> [T.Text]
splitOnChar char s = case T.dropWhile (== char) s of
                      "" -> []
                      s' -> w : splitOnChar char s''
                            where (w, s'') = T.break (== char) s'

splitObjects :: T.Text -> [T.Text]
splitObjects = splitOnChar ';'

splitFields :: T.Text -> [T.Text]
splitFields = splitOnChar ','

lexIdf :: T.Text -> [IdfObject]
lexIdf idfFile =  fmap splitFields (splitObjects (removeComments idfFile))

type IdfField  = T.Text
type IdfObject = [IdfField]
type IdfFile = [IdfObject]

data LexResult = LexResult { lexResultLineNum :: Int, lexResultMessage :: T.Text }

-- This signals a malformed field or object and should be considered a lexical error.
newLineAfterField :: IdfField -> Bool
newLineAfterField field =
    case T.find (== '\n') (T.dropWhile isSpace field) of
        Just _ -> True
        Nothing -> False

badFields = filter newLineAfterField (concat (lexIdf testFile))

data LintResult = LintResult { lineNum :: Int, message :: T.Text }

lintIdf :: T.Text -> T.Text
lintIdf idfFile = T.unlines $ filter newLineAfterField (concat $ lexIdf idfFile)

testFile :: T.Text
testFile = T.unlines
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
