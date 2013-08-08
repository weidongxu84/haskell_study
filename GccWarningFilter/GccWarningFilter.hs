import System.Environment
import Text.Regex.TDFA

main = do
    args <- getArgs
    case args of
        [inputFile, outputFile, patternFile] -> processGccLogByPattern inputFile outputFile patternFile
        _ -> putStrLn "Usage: GccWarningFilter GccLogFile FilteredWarningOutputFile FilterPatternFile"

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
    map converToPattern patterns
        where patterns = map (splitAt 2) (lines pattern)
              converToPattern =
                (\(action, regex) -> if action == "+ "
                    then RegexPattern RegexInclude regex
                    else if action == "- "
                        then RegexPattern RegexExclude regex
                        else RegexPattern RegexInclude "")

processLineByPattern :: [String] -> [RegexPattern] -> [String]
processLineByPattern lines patterns =
    filter (flip filterByPattern patterns) lines

filterByPattern :: String -> [RegexPattern] -> Bool
filterByPattern line patterns =
    foldl matchPattern True patterns
        where matchPattern =
                (\pass regexPattern -> if pass
                    then if action regexPattern == RegexInclude
                        then line =~ regex regexPattern
                        else not (line =~ regex regexPattern)
                    else False)
