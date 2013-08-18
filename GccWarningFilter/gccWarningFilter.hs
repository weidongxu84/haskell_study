#!/usr/bin/runhaskell
import System.Environment
import System.IO
import Data.List
import Text.Regex.TDFA

main = do
    args <- getArgs
    case args of
        [patternFile] -> processGccLogByPattern patternFile
        _ -> putStrLn $ "Read from standard input, write to standard output.\n"
                        ++ "Usage: gccWarningFilter FilterPatternFile"

data RegexAction = RegexInclude | RegexExclude deriving (Show, Enum, Eq)
data RegexPattern = RegexPattern { action :: RegexAction,
                                   regex :: String } deriving (Show)

processGccLogByPattern :: String -> IO ()
processGccLogByPattern patternFile = do
    patterns <- readFile patternFile
    interact $ processLineByPattern $ parsePattern patterns

processLineByPattern :: [RegexPattern] -> String -> String
processLineByPattern patterns =
    unlines . filter (flip filterByPattern patterns) . lines

parsePattern :: String -> [RegexPattern]
parsePattern pattern =
    map convertToPattern patterns
        where patterns = map (splitAt 2) (lines pattern)
              convertToPattern ("+ ", regex) = RegexPattern RegexInclude regex
              convertToPattern ("- ", regex) = RegexPattern RegexExclude regex
              convertToPattern (_, _) = RegexPattern RegexInclude ".*"  -- Invalid pattern file format

filterByPattern :: String -> [RegexPattern] -> Bool
filterByPattern line patterns =
    foldl' matchPattern True patterns
        where matchPattern False _ = False
              matchPattern True (RegexPattern RegexInclude regex) = line =~ regex
              matchPattern True (RegexPattern RegexExclude regex) = not (line =~ regex)
