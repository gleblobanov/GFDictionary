module Helpers where

import Text.ParserCombinators.Parsec
import GFDict
import Data.List (intercalate)

inFile1 = "Evaluation.txt"
inFile2 = "EvaluationResults.tsv"
outFile = "EvaluationsNew.tsv"
inFile3 = "EvaluationWedn.tsv"


doTransfer = do
  c1 <- readFile inFile1
  c2 <- readFile inFile2
  case parseTSV c1 of
    Left pe   -> error $ show pe
    Right c1' -> case parseTSV c2 of
      Left pe   -> error $ show pe
      Right c2' -> let res = zipWith transfer c1' c2'
                       out = (unlines . map (intercalate "\t")) res
                   in writeFile outFile out

transfer :: [String] -> [String] -> [String]
transfer ss1 ss2 = ss1 ++ [last ss2]

count = do
  c <- readFile inFile3
  case parseTSV c of
    Left pe -> error $ show pe
    Right r ->
      let tt = filter (\line->let true = line !! 5
                                  crct = line !! 8
                              in  true == "True" && crct == "correct") r
          ttl = fromIntegral $ length tt
          rl  = fromIntegral $ length r
          tf = filter (\line->let true = line !! 5
                                  crct = line !! 8
                              in  true == "True" && crct == "incorrect") r
          tfl = fromIntegral $ length tf
          ft = filter (\line->let true = line !! 5
                                  crct = line !! 8
                              in  true == "False" && crct == "correct") r
          ftl = fromIntegral $ length ft
          ff = filter (\line->let true = line !! 5
                                  crct = line !! 8
                              in  true == "False" && crct == "incorrect") r
          ffl = fromIntegral $ length ff
          ttr = filter (\line->let true = line !! 5
                                   crct = line !! 6
                               in  true == "True" && crct == "True") r
          ttlr = fromIntegral $ length ttr
          tfr = filter (\line->let true = line !! 5
                                   crct = line !! 6
                               in  true == "True" && crct == "False") r
          tflr = fromIntegral $ length tfr
          ftr = filter (\line->let true = line !! 5
                                   crct = line !! 6
                               in  true == "False" && crct == "True") r
          ftlr = fromIntegral $ length ftr
          ffr = filter (\line->let true = line !! 5
                                   crct = line !! 6
                               in  true == "False" && crct == "False") r
          fflr = fromIntegral $ length ffr
      in putStrLn $ "All: " ++ (show rl) ++ "\n" ++
                  "Pred True & My True: " ++ (show  ttl) ++ " (" ++ (show $ 100 / rl * ttl) ++ "%)" ++ "\n" ++
                  "Pred True & My False: " ++ (show tfl) ++ " (" ++ (show $ 100 / rl * tfl) ++ "%)" ++ "\n" ++
                  "Pred False & My True: " ++ (show ftl) ++ " (" ++ (show $ 100 / rl * ftl) ++ "%)" ++ "\n" ++
                  "Pred False & My False: " ++ (show ffl) ++ " (" ++ (show $ 100 / rl * ffl) ++ "%)" ++ "\n" ++ "Pred True & Rnd True: " ++ (show  ttlr) ++ " (" ++ (show $ 100 / rl * ttlr) ++ "%)" ++ "\n" ++
                  "Pred True & Rnd False: " ++ (show tflr) ++ " (" ++ (show $ 100 / rl * tflr) ++ "%)" ++ "\n" ++
                  "Pred False & Rnd True: " ++ (show ftlr) ++ " (" ++ (show $ 100 / rl * ftlr) ++ "%)" ++ "\n" ++
                  "Pred False & Rnd False: " ++ (show fflr) ++ " (" ++ (show $ 100 / rl * fflr) ++ "%)" ++ "\n"

countWords = do
  c <- readFile inFile3
  case parseTSV c of
    Left pe -> error $ show pe
    Right r -> (print . length . rmdup) [head l | l <- r]

rmdup [] = []
rmdup (x:xs) | x `elem` xs = rmdup xs
             | otherwise   = x : rmdup xs
