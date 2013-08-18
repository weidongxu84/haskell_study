#!/usr/bin/runhaskell
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
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
    handleJust (\e -> if isEOFError e then Just () else Nothing)
               (\e -> return ())
               (processLineByPattern $ parsePattern patterns)

processLineByPattern :: [RegexPattern] -> IO ()
processLineByPattern patterns = do
    line <- getLine
    if filterByPattern line patterns
    then putStrLn line
    else return ()
    processLineByPattern patterns

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
