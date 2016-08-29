module Main where

import Prelude
import GFDict (parseTSV)
import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.List
import qualified Data.List.Split as S
import System.Random

rusFreq  = "evaluationData/ru.txt"
engFreq  = "evaluationData/en.txt"
predFile = "evaluationData/predictions.tsv"
dataNoun = "evaluationData/dict/data.noun"
dataAdv = "evaluationData/dict/data.adv"
dataAdj = "evaluationData/dict/data.adj"
dataVerb = "evaluationData/dict/data.verb"
mostNumber = 400


data Rec = Rec {
               getOffset :: String,
               getLemma  :: String,
               getPoS    :: String,
               getIsTr   :: Bool,
               gloss     :: Maybe String,
               getCount  :: Int}
           deriving Eq

setCount (Rec o l p i g _) = Rec o l p i g

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
  n' <- readFile dataNoun
  r' <- readFile dataAdv
  v' <- readFile dataVerb
  a' <- readFile dataAdj
  let c' = case parseTSV c of
        Left pe -> error $ show pe
        Right d -> filter (\a -> length a >= 5) d
      n = makeDict n'
      r = makeDict r'
      v = makeDict v'
      a = makeDict a'
      predDataRus = map (getPredRecord Rus n r v a) c'
      predDataEng = map (getPredRecord Eng n r v a) c'
  return (predDataRus, predDataEng)

makeDict :: String -> [(String, String)]
makeDict ss = map (\s -> (head $ words s, init $ init $ (S.splitOn "|" s) !! 1)) ss'
  where ss' = drop 29 $ lines ss

getPredRecord :: Lang ->
                 [(String, String)] -> -- nouns (PID, gloss)
                 [(String, String)] -> -- adverbs
                 [(String, String)] -> -- verbs
                 [(String, String)] -> -- adjectives
                 [String] ->
                 (String, Rec)
getPredRecord l n r v a ss =
  let isTransl = ("True" ==) $ last ss
      offset   = head ss
      pos      = [head ss !! 4]
      predEng  = ss !! 2
      predRus  = ss !! 4
      glossDict = case pos of
                                         "n" -> n
                                         "r" -> r
                                         "v" -> v
                                         "a" -> a
      gloss     = lookup (drop 5 offset) glossDict
  in case l of
    Eng -> (predEng, Rec offset predRus pos isTransl gloss 0)
    Rus -> (predRus, Rec offset predEng pos isTransl gloss 0)


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


getMostFreq :: PredData -> FreqData -> Int -> PredData -> PredData
getMostFreq predDataIn freqData num predDataOut
  | num == mostNumber = predDataOut
  | otherwise = let toCheck = head freqData
                    predRec = lookup toCheck predDataIn
                in case predRec of
      Just a  -> getMostFreq (delete (toCheck, a) predDataIn) freqData num' ((toCheck, (setCount a num')) : predDataOut)
        where num' | null predDataOut = num + 1
                   | (fst . head) predDataOut == toCheck = num
                   | otherwise = num + 1
      Nothing -> getMostFreq predDataIn (tail freqData) num predDataOut

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
  -- rus <- getFreqData rusFreq
  eng <- getFreqData engFreq
  gen <- newStdGen
  let --rusMostFreq = getMostFreq predDataRus rus []
      engMostFreq = getMostFreq predDataEng eng 0 []
      bools = randoms gen :: [Bool]
      withBools = zip engMostFreq (take (length engMostFreq) bools)
      toPrint = reverse withBools
      -- mostFreq    = mergePredData rusMostFreq engMostFreq
      out = concatMap (\((a, Rec off b c d gl i), bl) ->
                        show i  ++ "\t" ++
                        off     ++ "\t" ++
                        a       ++ "\t" ++
                        b       ++ "\t" ++
                        show c  ++ "\t" ++
                        show d  ++ "\t" ++
                        show bl ++ "\t" ++ "|" ++
                        fromJust gl ++ "\n") toPrint
  writeFile "Evaluation.txt" $ out
  return ()
