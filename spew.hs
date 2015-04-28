module Main where

import HaskalCommon

import Control.Functor
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
            return $ dropWhile (\s -> last s /= '.') (evalState (runModel model) gen)
            let ws = take numWords (evalState (runModel model) gen)
            putStr $ lineFill 72 ws

usage :: IO ()
usage = do
    progName <- getProgName
    hPutStrLn stderr $ "usage: " ++ progName ++ " numWords"
    exitWith $ ExitFailure 1

mkFastModel :: String -> FastModel
mkFastModel modelString =  listArray (1, length modelData) modelData where
    modelData = read <$> lines modelString :: ProcessedModel

select :: [(Int, Int)] -> RandState Int
select tuples = getNext tuples <$> (state . randomR $ (0, range-1)) where
    range = foldr ((+) . fst) 0 tuples
    getNext (tuple:rest) num
        | num <= (fst tuple) = snd tuple
        | otherwise          = getNext rest (num - (fst tuple))


runModel :: FastModel -> RandState [String]
runModel model = iter start where
    iter ix = do
        let (word, successors) = model ! ix
        case successors of
            [] -> return [word]
            _  -> do
                nextIx <- select successors
                ws <- iter nextIx
                return (word:ws)
    start = 1

lineFill :: Int -> [String] -> String
lineFill _ [] = "\n"
lineFill n (x:xs) = iter x xs where
    iter x (y:ys)
        | length x + length y + 1 > n = x ++ "\n" ++ lineFill n (y:ys)
        | otherwise                   = iter (x ++ " " ++ y) ys
    iter x [] = x ++ "\n"
