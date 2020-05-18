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
    if help options
        then putStrLn "idf-lint\n\nUSAGE: ep-lint [options].."
        else do
             let path = filepath options
             fileExists <- doesFileExist path
             if fileExists
             then do
                  idfFile <- TIO.readFile path
                  TIO.putStrLn idfFile
             else putStrLn $ path ++ "Not found."

parseArgs :: [String] -> EpLintOptions -> EpLintOptions
parseArgs (x:xs) options
            | x == "-h" || x == "--help" = parseArgs xs (options { help = True })
            | otherwise                  = parseArgs xs (options { filepath = x })

parseArgs [] options = options

data EpLintOptions = EpLintOptions {
                                     help :: Bool,
                                     filepath :: String
                                   } deriving Show

defaultLintOptions = EpLintOptions { help = False, filepath = "in.idf" }


removeCommentLine :: T.Text -> T.Text
removeCommentLine line = T.takeWhile (/= '!') line

removeComments :: T.Text -> T.Text
removeComments idfFile = (T.unlines . (map removeCommentLine) . T.lines) idfFile


splitOnChar :: Char -> T.Text -> [T.Text]
splitOnChar char s = case T.dropWhile (== char) s of
                      "" -> []
                      s' -> w : splitOnChar char s''
                            where (w, s'') = T.break (== char) s'

splitObjects :: T.Text -> [T.Text]
splitObjects idfFile = splitOnChar ';' idfFile

splitFields :: T.Text -> [T.Text]
splitFields idfObject = splitOnChar ',' idfObject

lexIdf :: T.Text -> [IdfObject]
lexIdf idfFile =  fmap splitFields (splitObjects (removeComments idfFile))

type IdfField  = T.Text
type IdfObject = [IdfField]

data LexResult = LexResult { lexResultLineNum :: Int, lexResultMessage :: T.Text }

-- This signals a malformed field or object and should be considered a lexical error.
newLineAfterField :: T.Text -> Bool
newLineAfterField field =
    case T.find (== '\n') (T.dropWhile isSpace field) of
        Just _ -> True
        Nothing -> False

badFields = filter newLineAfterField (concat (lexIdf testFile))

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
