module Main where

import Paths_haskal

import Control.Monad
import Data.Char
import Data.Map (Map, fromListWith)
import Network.HTTP
import System.IO
import Text.HTML.TagSoup
import Text.StringLike

type PrimitiveModel = Map (String,String) [String]
type ProcessedModel = [(String,[(Int,Int)])]

main :: IO ()
main = do
    urls <- getDataFileName "urls.txt"
    urlList <- lines <$> readFile urls
    pages <- sequence $ (fmap parseTags . openURL) <$> urlList
    let paragraphText = (innerText . getPTags) <$> pages
    let corpus = toLower <$> (filter (\c -> (isAlpha c || isSpace c) && isAscii c) $ concat paragraphText)
    print $ mkPrimitiveModel corpus

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

mkPrimitiveModel :: String -> PrimitiveModel
mkPrimitiveModel corpus = fromListWith (++) (secondOrderWordRelations $ filter (/= "") $ words corpus) where
    secondOrderWordRelations (first:second:third:rest) =
        foldr mkSecondOrderList [((first, second), [third])] rest
    mkSecondOrderList word acc@(((_, prevSecond), [prevThird]):rest) =
        ((prevSecond, prevThird), [word]) : acc
