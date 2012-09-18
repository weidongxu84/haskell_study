calcRPN :: String -> Float
calcRPN = head . foldl processInput [] . words
    where processInput (x:x':xs) "+" = (x' + x):xs
          processInput (x:x':xs) "-" = (x' - x):xs
          processInput (x:x':xs) "*" = (x' * x):xs
          processInput (x:x':xs) "/" = (x' / x):xs
          processInput xs floatString = (read floatString):xs

main = do
    input <- getLine
    putStrLn $ show (calcRPN input)
    main
