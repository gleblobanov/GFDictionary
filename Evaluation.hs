module Main where

import Prelude
import GFDict (parseTSV)
import Text.ParserCombinators.Parsec
import Data.Maybe


rusFreq  = "evaluationData/ru.txt"
engFreq  = "evaluationData/en.txt"
predFile = "evaluationData/predictions.tsv"
mostNumber = 400


data Rec = Rec {
               getLemma :: String,
               getPoS   :: String,
               getIsTr  :: Bool}

-- | A key-value storage of translation pairs. Two types of the storage are
-- used: one has Russian lemmas as keys, and another has English lemmas as keys.
-- Both have tuples as values, which contain an another language lemma and a
-- flag denoting if it is a correct translation.
type PredData = [(String, Rec)]

type FreqData = [String]

data Lang = Eng | Rus


getPredData :: IO (PredData, PredData)
getPredData = do
  c <- readFile predFile
  let c' = case parseTSV c of
        Left pe -> error $ show pe
        Right d -> filter (\a -> length a >= 5) d
      predDataRus = map (getPredRecord Rus) c'
      predDataEng = map (getPredRecord Eng) c'
  return (predDataRus, predDataEng)


getPredRecord :: Lang -> [String] -> (String, Rec)
getPredRecord l ss =
  let isTransl = ("True" ==) $ last ss
      pos      = [head ss !! 4]
      predEng  = ss !! 2
      predRus  = ss !! 4
  in case l of
    Eng -> (predEng, Rec predRus pos isTransl)
    Rus -> (predRus, Rec predEng pos isTransl)


getFreqData :: String -> IO FreqData
getFreqData filePath = do
  c <- readFile filePath
  let c' = case parseFreq c of
        Left pe -> error $ show pe
        Right d -> d
      freqData = map head c'
  return freqData


freqFile :: GenParser Char st [[String]]
freqFile = sepBy freqLine (string "\r\n")

freqLine :: GenParser Char st [String]
freqLine = sepBy freqCell (char ' ')

freqCell :: GenParser Char st String
freqCell = many (noneOf " \r\n")

eol :: GenParser Char st Char
eol = char '\n'

parseFreq :: String -> Either ParseError [[String]]
parseFreq = parse freqFile "(unknown)"


getMostFreq :: PredData -> FreqData -> PredData -> PredData
getMostFreq predDataIn freqData predDataOut
  | length predDataOut == mostNumber = predDataOut
  | otherwise = let toCheck = head freqData
                    predRec = lookup toCheck predDataIn
                    predDataOut' = case predRec of
                      Just a  -> (toCheck, a) : predDataOut
                      Nothing -> predDataOut
                in getMostFreq predDataIn (tail freqData) predDataOut'


mergePredData :: PredData -> PredData -> PredData
mergePredData []  pD2 = pD2
mergePredData (p1:pD1) pD2 =
  let lemma = getLemma $ snd p1
      rest  = mergePredData pD1 pD2
  in if isJust $ lookup lemma pD2
     then rest
     else p1 : rest


main = do
  (predDataRus, predDataEng) <- getPredData
  rus      <- getFreqData rusFreq
  eng      <- getFreqData engFreq
  let rusMostFreq = getMostFreq predDataRus rus []
      engMostFreq = getMostFreq predDataEng eng []
      mostFreq    = mergePredData rusMostFreq engMostFreq
      out = concatMap (\(a, Rec b c d) -> a ++ "\t" ++ b ++ "\t" ++ show c ++ "\t" ++ show d ++ "\n") mostFreq
  writeFile "Evaluation.txt" $ out
  return ()
