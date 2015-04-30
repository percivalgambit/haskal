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

-- Fetch a page from the specified url
openURL :: String -> IO String
openURL url = getResponseBody =<< simpleHTTP (getRequest url)

-- Filter a list of tags so only the tags within a <p> block remain
harvest :: StringLike str => [Tag str] -> [Tag str]
harvest []   = []
harvest tags = inPTag ++ harvest rest where
    (inPTag, rest) = span (~/= TagClose "p") $ dropWhile (~/= TagOpen "p" []) tags

-- Filter all non-alpha characters (except for .) out of a piece of text and
-- split the text on spaces into separate words
normalizeWords :: String -> [String]
normalizeWords = filterHasLetter . filterBadChars . words where
    filterBadChars = fmap (filter (\c -> isAscii c && isAlpha c || c=='.'))
    filterHasLetter = filter (any isAlpha)

-- Take in a list containing a list of words from each downloaded page and
-- fold through the lists to create a primitive model
mkPrimitiveModel :: [[String]] -> PrimitiveModel
mkPrimitiveModel normalizedCorpus = fromListWith (++) relationLists where
    relationLists = normalizedCorpus >>= foldr mkWordRelations [] . tails
    mkWordRelations (firstWord:secondWord:thirdWord:_) =
        (:) ((firstWord, secondWord), [thirdWord])
    mkWordRelations _ = id

-- Group a list of words into a (frequency, word) pair from largest frequency to
-- smallest frequency
groupByWord :: [String] -> [(Int, String)]
groupByWord words =
    reverse . sort $ zip (fmap length groupedWords) (fmap head groupedWords) where
        groupedWords = group words

-- Take in a primitive model and turn it into a processed model by folding
-- through it and grouping the successor words by frequency
mkProcessedModel :: PrimitiveModel -> ProcessedModel
mkProcessedModel primitiveModel = foldrWithKey foldModel [] primitiveModel where
    foldModel (firstW, secondW) succesors = (:) (secondW, nextIndices) where
        nextIndices = [ (count, index)
                      | (count, thirdW) <- groupByWord succesors
                      , (Just index) <- [(secondW, thirdW) `lookupIndex` primitiveModel]
                      ]


-- Serialize a processed model to a file by writing each entry in the list
-- of the processed model on a separate line of the file
serializeModel :: ProcessedModel -> Handle -> IO ()
serializeModel model fileHandle = do
    sequence $ (hPutStrLn fileHandle . show) <$> model
    return ()
