module Main where

import Paths_haskal

import Control.Monad
import Data.Map
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
    print $ innerText $ getPTags (pages !! 0)


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
