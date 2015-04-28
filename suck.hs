module Main where

import HaskalCommon
import Paths_haskal

import Control.Monad
import Data.Char
import Data.List
import Data.Map ((!), fromListWith, keys, lookupIndex, map, Map)
import Network.HTTP
import System.IO
import Text.HTML.TagSoup
import Text.StringLike

type PrimitiveModel = Map (String,String) [String]

main :: IO ()
main = do
    urls <- getDataFileName "urls.txt"
    urlList <- lines <$> readFile urls
    pages <- sequence $ (fmap parseTags . openURL) <$> urlList
    let corpus = (innerText . getPTags) <$> pages
    let primitiveModel = mkPrimitiveModel corpus
    let processedModel = mkProcessedModel $ Data.Map.map groupByWord primitiveModel
    withFile serializedFile WriteMode (serializeModel processedModel)

openURL :: String -> IO String
openURL url = getResponseBody =<< simpleHTTP (getRequest url)

getPTags [] = []
getPTags (currTag:rest) = if (isTagOpenName "p" currTag)
                          then inPTag rest
                          else getPTags rest where
    inPTag [] = []
    inPTag (currTag:rest) = if (isTagCloseName "p" currTag)
                            then getPTags rest
                            else currTag : inPTag rest

mkPrimitiveModel :: [String] -> PrimitiveModel
mkPrimitiveModel corpus = fromListWith (++) (concat $ (secondOrderWordRelations . filter (/= "")) <$> normalizedCorpus) where
    secondOrderWordRelations (first:second:third:rest) =
        foldr mkSecondOrderList [((first, second), [third])] rest
    mkSecondOrderList word acc@(((_, prevSecond), [prevThird]):rest) =
        ((prevSecond, prevThird), [word]) : acc
    normalizedCorpus = [ normalizedPaper
                       | paper <- corpus
                       , let paperWords = words $ paper
                       , let normalizedPaper = (fmap toLower . filter isAscii) <$> paperWords
                       ]

groupByWord :: [String] -> [(Int, String)]
groupByWord words = sort $ zip (fmap length groupedWords) (fmap head groupedWords) where
    groupedWords = group words

mkProcessedModel :: Map (String,String) [(Int, String)] -> ProcessedModel
mkProcessedModel countMap = [ (secondW, nextIndices)
                            | let words = keys countMap
                            , entry@(w, secondW) <- words
                            , let wCounts = countMap ! entry
                            , let nextIndices = [ (count, index)
                                                | (count, thirdW) <- wCounts
                                                , (Just index) <- [(secondW, thirdW) `lookupIndex` countMap]
                                                ]
                            ]

serializeModel :: ProcessedModel -> Handle -> IO ()
serializeModel model fileHandle = do
    sequence $ (hPutStrLn fileHandle . show) <$> model
    return ()
