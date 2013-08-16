#!/usr/bin/runhaskell
import System.Environment
import Text.XML.HXT.Core

main = do
    args <- getArgs
    case args of
        [translationXmlFile] -> processFile translationXmlFile
        _ -> putStrLn "Usage: deleteObsoleteTranslation TranslationXmlFile"

processFile :: String -> IO()
processFile xmlFile = do
    runX(readDocument [withValidate no, withInputEncoding utf8] xmlFile
         >>>
         processChildren (deleteObsoleteTranslation `when` isElem)
         >>>
         writeDocument [withIndent yes, withOutputEncoding utf8] xmlFile)
    return()

deleteObsoleteTranslation :: IOSArrow XmlTree XmlTree
deleteObsoleteTranslation =
    processTopDown(none `when` isObsoleteMessageElem)
        where isObsoleteMessageElem = isElem >>> hasName "message"
                                      >>> getChildren
                                      >>> hasName "translation"
                                      >>> hasAttr "type" >>> getAttrValue "type" >>> isA (== "obsolete")
