module Main where

import HaskalCommon

import Data.Functor
import Control.Monad.State
import Data.Array
import System.Environment
import System.Exit
import System.IO
import System.Random

type FastModel = Array Int FrequencyTuple
type RandState = State StdGen

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> usage
        _  -> do
            let numWords = read $ head args :: Int
            model <- mkFastModel <$> readFile serializedFile
            gen <- newStdGen
            let (start, gen') = randomR (bounds model) gen
            let ws = take numWords
                     $ drop 1
                     $ dropWhile (\s -> last s /= '.')
                                 (evalState (runModel model start) gen')
            putStr $ lineFill 80 ws

-- usage message if no arguments are given to the program
usage :: IO ()
usage = do
    progName <- getProgName
    hPutStrLn stderr $ "usage: " ++ progName ++ " numWords"
    exitWith $ ExitFailure 1

-- Take in a string read from a serialized processed model and turn it into a
-- FastModel by storing the processed model in an array
mkFastModel :: String -> FastModel
mkFastModel modelString =  listArray (1, length modelData) modelData where
    modelData = read <$> lines modelString :: ProcessedModel

-- Take in a list of (weight, next index) pairs and choose one by weight,
-- returning a RandState that contains the next index
select :: [(Int, Int)] -> RandState Int
select tuples = getNext tuples <$> (state . randomR $ (0, range-1)) where
    range = foldr ((+) . fst) 0 tuples
    getNext (tuple:rest) num
        | num <= (fst tuple) = snd tuple
        | otherwise          = getNext rest (num - (fst tuple))

-- Run the FastModel using a specified start state.  The model is run by choosing
-- an infinite number of words by choosing a random successor from the list of
-- successors of the current state in the FastModel
runModel :: FastModel -> Int -> RandState [String]
runModel model start = iter start where
    iter ix = do
        let (word, successors) = model ! ix
        case successors of
            [] -> return [word]
            _  -> do
                nextIx <- select successors
                ws <- iter nextIx
                return (word:ws)

-- Pretty-print a string using n-character lines (Taken from CMSC 162 Lecture 18)
lineFill :: Int -> [String] -> String
lineFill _ [] = "\n"
lineFill n (x:xs) = iter x xs where
    iter x (y:ys)
        | length x + length y + 1 > n = x ++ "\n" ++ lineFill n (y:ys)
        | otherwise                   = iter (x ++ " " ++ y) ys
    iter x [] = x ++ "\n"
