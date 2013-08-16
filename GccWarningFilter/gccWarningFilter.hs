#!/usr/bin/runhaskell
import System.Environment
import Data.List
import Text.Regex.TDFA

main = do
    args <- getArgs
    case args of
        [inputFile, outputFile, patternFile] -> processGccLogByPattern inputFile outputFile patternFile
        _ -> putStrLn "Usage: gccWarningFilter GccLogFile FilteredWarningOutputFile FilterPatternFile"

data RegexAction = RegexInclude | RegexExclude deriving (Show, Enum, Eq)
data RegexPattern = RegexPattern { action :: RegexAction,
                                   regex :: String } deriving (Show)

processGccLogByPattern :: String -> String -> String -> IO ()
processGccLogByPattern inputFile outputFile patternFile = do
    input <- readFile inputFile
    pattern <- readFile patternFile
    writeFile outputFile $ unlines $ processLineByPattern (lines input) (parsePattern pattern)

parsePattern :: String -> [RegexPattern]
parsePattern pattern =
    map convertToPattern patterns
        where patterns = map (splitAt 2) (lines pattern)
              convertToPattern ("+ ", regex) = RegexPattern RegexInclude regex
              convertToPattern ("- ", regex) = RegexPattern RegexExclude regex
              convertToPattern (_, _) = RegexPattern RegexInclude ".*"  -- Invalid pattern file format

processLineByPattern :: [String] -> [RegexPattern] -> [String]
processLineByPattern lines patterns =
    filter (flip filterByPattern patterns) lines

filterByPattern :: String -> [RegexPattern] -> Bool
filterByPattern line patterns =
    foldl' matchPattern True patterns
        where matchPattern False _ = False
              matchPattern True (RegexPattern RegexInclude regex) = line =~ regex
              matchPattern True (RegexPattern RegexExclude regex) = not (line =~ regex)
