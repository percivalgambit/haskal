module Main where

import Paths_haskal

import Control.Monad
import Data.Map
import Network.HTTP
import System.IO
import Text.HTML.TagSoup

type PrimitiveModel = Map (String,String) [String]
type ProcessedModel = [(String,[(Int,Int)])]

main :: IO ()
main = do
    urls <- getDataFileName "urls.txt"
    urlList <- lines <$> readFile urls
    pages <- getPages urlList
    let pageTexts = (innerText . parseTags) <$> pages
    return ()

getPages :: [String] -> IO [String]
getPages urlList = do
    let requests = (simpleHTTP . getRequest) <$> urlList
    mapM (>>= getResponseBody) requests
