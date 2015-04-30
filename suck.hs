module Main where

import HaskalCommon
import Paths_haskal

import Control.Monad
import Data.Char
import Data.Functor
import Data.List
import Data.Map hiding (foldr, filter, null)
import Network.HTTP
import System.IO
import Text.HTML.TagSoup
import Text.StringLike

type PrimitiveModel = Map (String,String) [String]

main :: IO ()
main = do
    urlFile <- getDataFileName "urls.txt"
    urlList <- lines <$> readFile urlFile
    pages <- sequence $ (fmap parseTags . openURL) <$> urlList
    let corpus = (normalizeWords . innerText . harvest) <$> pages
    let primitiveModel = mkPrimitiveModel corpus
    let processedModel = mkProcessedModel primitiveModel
    withFile serializedFile WriteMode (serializeModel processedModel)

openURL :: String -> IO String
openURL url = getResponseBody =<< simpleHTTP (getRequest url)

harvest :: StringLike str => [Tag str] -> [Tag str]
harvest []   = []
harvest tags = inPTag ++ harvest rest where
    (inPTag, rest) = span (~/= TagClose "p") $ dropWhile (~/= TagOpen "p" []) tags

normalizeWords :: String -> [String]
normalizeWords = filterHasLetter . filterBadChars . words where
    filterBadChars = fmap (filter (\c -> isAscii c && isAlpha c || c=='.'))
    filterHasLetter = filter (any isAlpha)

mkPrimitiveModel :: [[String]] -> PrimitiveModel
mkPrimitiveModel normalizedCorpus = fromListWith (++) relationLists where
    relationLists = normalizedCorpus >>= foldr mkWordRelations [] . tails
    mkWordRelations (firstWord:secondWord:thirdWord:_) = (:) ((firstWord, secondWord), [thirdWord])
    mkWordRelations _ = id

groupByWord :: [String] -> [(Int, String)]
groupByWord words = reverse . sort $ zip (fmap length groupedWords) (fmap head groupedWords) where
    groupedWords = group words

mkProcessedModel :: Map (String,String) [String] -> ProcessedModel
mkProcessedModel primitiveModel = foldrWithKey foldModel [] primitiveModel where
    foldModel (firstW, secondW) succesors = (:) (secondW, nextIndices) where
        nextIndices = [ (count, index)
                      | (count, thirdW) <- groupByWord succesors
                      , (Just index) <- [(secondW, thirdW) `lookupIndex` primitiveModel]
                      ]

serializeModel :: ProcessedModel -> Handle -> IO ()
serializeModel model fileHandle = do
    sequence $ (hPutStrLn fileHandle . show) <$> model
    return ()
