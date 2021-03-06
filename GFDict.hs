{-|
Module      : GFDict
Description : Script generating GF translation dictionary from data received
              by bridging Princeton WordNet and Russian segment of Open Multilingual WordNet.
Author      : Gleb Lobanov
Maintainer  : mail@gleblobanov.ru
-}
module GFDict where

import qualified PGF2
import Data.Char
import Data.List
import Data.Ord
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Parallel.Strategies

-- | Results of heuristics.
predictionsTSV :: String
predictionsTSV = "engrus/predictions.tsv"

-- | Abstract syntax for offline translation app.
dictionaryGF :: String
dictionaryGF = "engrus/Dictionary.gf"

-- | Russian concrete syntax for offline translation app.
dictionaryRus = "engrus/DictionaryRus.gf"

-- | Compiled grammar.
dictionaryPGF :: String
dictionaryPGF = "engrus/Dictionary.pgf"

-- | Russian abstract syntax.
dictRusPGF :: String
dictRusPGF = "engrus/DictRusAbs.pgf"

-- | Concrete English syntax name for offline translation app.
concEng = "DictionaryEng"

-- | Name of a variant of concrete Russian syntax.
concRus = "DictRus"

-- | A variant of concrete Russian syntax.
concRusFile = "engrus/DictRus.gf"

-- | WordNet lemma.
type Lemma = String

-- | A line of the file with results of heuristics.
data PredRecord = PredRecord { predWNId :: String,
                               predPS   :: PoS,
                               predEng  :: Lemma,
                               predRus  :: Lemma }
                  deriving (Show)

-- | Heuristics data.
type PredData = [PredRecord]


-- | GF abstract syntax id for the offline translation app grammar.
type AbsId = String

-- | Princeton WordNet offset.
type AbsWNId = String

-- | Record binding GF abstract syntax id and Princeton WordNet offset.
data AbsRecord = AbsRecord { absId   :: AbsId,
                             absPS   :: PoS,
                             absWNId :: AbsWNId }
                 deriving (Eq, Ord, Show)

-- | Map of GF abstract ids to records with their abstract syntax ids and offsets.
type AbsData = Map.Map WordForm AbsRecord

-- | Word form.
type WordForm = String


-- | GF classification of parts of speech.
data PoS = V | V2 | V3 | VV | VS | VQ | VA | V2V | V2S | V2Q | V2A
         | A | A2
         | N | N2 | N3 | PN
         | Adv | AdV | AdA | AdN | IAdv | CAdv
         | UndefPoS
         deriving (Show, Eq, Ord)

-- | Checks if an argument is a verb.
isVerb p = p `elem` [V, V2, V3, VV, VS, VQ, VA, V2V, V2S, V2Q, V2A]

-- | Checks if an argument is an adjective.
isAdj p  = p `elem`  [A, A2]

-- | Checks if an argument is a noun.
isNoun p = p `elem` [N, N2, N3, PN]

-- | Checks if an argument is an adverb.
isAdv p  = p `elem` [Adv, AdV, AdA, AdN, IAdv, CAdv]

-- | Translates GF classification of parts of speech to WordnNet classification.
simplify :: PoS -> PoS
simplify x | isVerb x  = V
           | isAdj x   = A
           | isNoun x  = N
           | isAdv x   = Adv
           | otherwise = UndefPoS

-- | Parses parts of speech both GF and WordNet.
parsePoS :: String -> PoS
parsePoS x = case x of
  "v"   -> V
  "V"   -> V
  "V2"  -> V2
  "V3"  -> V3
  "VV"  -> VV
  "VS"  -> VS
  "VQ"  -> VQ
  "VA"  -> VA
  "V2V" -> V2V
  "V2S" -> V2S
  "V2Q" -> V2Q
  "V2A" -> V2A

  "a"  -> A
  "A"  -> A
  "A2" -> A2

  "n"  -> N
  "N"  -> N
  "N2" -> N2
  "N3" -> N3
  "PN" -> PN

  "r"    -> Adv
  "Adv"  -> Adv
  "AdV"  -> AdV
  "AdA"  -> AdA
  "AdN"  -> AdN
  "IAdv" -> IAdv
  "CAdv" -> CAdv

  _      -> UndefPoS

-- | Parser of tab-separated file.
tsvFile :: GenParser Char st [[String]]
tsvFile = sepBy line eol

-- | Parser of line of tab-separated file.
line :: GenParser Char st [String]
line = sepBy cell (char '\t')

-- | Parser of cell of line of tab-separated file.
cell :: GenParser Char st String
cell = many (noneOf "\t\n")

-- | Parser of end of line.
eol :: GenParser Char st Char
eol = char '\n'

-- | TSV parser run function.
parseTSV :: String -> Either ParseError [[String]]
parseTSV = parse tsvFile "(unknown)"


-- | Parses heuristics data.
getPredData :: IO PredData
getPredData = do
  contentPred  <- readFile predictionsTSV         -- Read a file with heuristics.
  let contentPred' = case parseTSV contentPred of -- Parse the file.
        Left pe -> error $ show pe
        Right d -> d
      predData = map getRecord $ filter isTransl contentPred' -- Extract data from the file.
        where isTransl = ("True" ==) . last                   -- Choose only the translation pairs that algorithms marked correct.
              getRecord ss = PredRecord { predWNId = filter isDigit (head ss), -- Get Princeton offset.
                                          predPS    = parsePoS [head ss !! 4], -- Get WordNet part of speech.
                                          predEng   = ss !! 2,                 -- English word form.
                                          predRus   = ss !! 4 }                -- Russian word form.
  return predData


-- | Parses GF abstract syntax data.
-- Every line contains both abstract id and Princeton WordNet offset.
getAbsData :: IO AbsData
getAbsData = do contentDict <- readFile dictionaryGF -- Read a file with GF abstract syntax.
                let dict = map t $ filter isLong $ map words $ lines contentDict -- Parse the file.
                      where isLong l   = length l > 6 -- Choose only lines containing information, which are longer then 6 chars.
                            absIdPos   = 1            -- The positions of the abstract ids.
                            absPSPos   = 3            -- The positions of the parts of speech.
                            absWNIdPos = 6            -- The position of Princeton offsets.
                            t ws = AbsRecord { absId   = ws !! absIdPos, -- Create a record with information retrieved from the line.
                                               absPS   = parsePoS $ ws !! absPSPos,
                                               absWNId = ws !! absWNIdPos }
                    absData = foldl insertAbs Map.empty dict -- Map abstract ids to records that contain them.
                      where insertAbs m r = Map.insert (absId r) r m --  Create a (key, value) record for the mapping.
                return absData


-- | Extracts Russian word forms from a Russian concrete syntax.
getRusWFs :: IO [(String, (String, PoS))]
getRusWFs = do contentDict <- readFile concRusFile -- Get content of the file with Russian concrete syntax.
               let dict = map t $ filter isLin $ map words $ lines contentDict -- Parse the file.
                      where isLin l | null l    = False -- Checks if a line contains a linearization function.
                                    | otherwise = head l == "lin"
                            idPos   = 1 -- Position of abstract syntax id that is linearized.
                            toDrop  = 3 -- Drop unnecessary symbols.
                            t ws = let id' = ws !! idPos -- Get abstract id.
                                   in (id', (unwords (drop toDrop ws), -- Get Russian word forms for the abstract id.
                                             parsePoS $ last $ splitOn "_" id')) -- Get a part of speech.
               return dict

-- | Generates translation dictionary.
main :: IO ()
main = do
  predData <- getPredData -- Get heuristics data.
  absData  <- getAbsData  -- Get GF abstract syntax data.
  grEng    <- PGF2.readPGF dictionaryPGF -- Get English grammar.
  grRus    <- PGF2.readPGF dictRusPGF    -- Get Russian grammar.
  rusWFs   <- getRusWFs -- Get data from a Russian concrete syntax.
  let d = map f predData -- Transform heuristics data.
        where f predRec = do
                morphsEng <- lum grEng (predEng predRec) concEng -- Get inflection information.
                morphsRus <- lum grRus (predRus predRec) concRus
                let absIdRus' [] = ""
                    absIdRus' ((id,_,_):_) = id
                    absIdRus = absIdRus' morphsRus
                return
                  $ map addRusWFs
                  $ filter (\(_,x,_,_) -> x /= "")
                  $ filter (\(_,_,_,x) -> x /= UndefPoS)
                  $ map (checkWNId absData predRec)
                  $ map (getAbsIdEn absIdRus) morphsEng
                  where getAbsIdEn absIdRus (absIdEng, _, _) = (absIdEng,
                                                       absIdRus,
                                                       predRus predRec)
                        addRusWFs (absIdEng, absIdRus, lemma, ps) = (absIdEng,
                                                                 absIdRus,
                                                                 lemma,
                                                                 wfs)
                          where wfs :: String
                                wfs = let sp = fromMaybe ("", UndefPoS) $ lookup absIdRus rusWFs
                                          p  = snd sp
                                          s  = fst sp
                                      in if (simplify ps /= simplify p || p == PN)
                                         then ""
                                         else case ps of
                                  V2 -> "mkV2 (" ++ init s ++ ") \"\" accusative ;"
                                  V3 -> "mkV3 (" ++ init s ++ ") \"\" \"\" accusative\
                                                              \ dative;"
                                  VS -> "mkVS (" ++ init s ++ ") ;"
                                  VQ -> "mkVQ (" ++ init s ++ ") ;"
                                  V2V -> "mkV2V (" ++ init s ++ ") \"\" accusative ;"
                                  V2S -> "mkV2S (" ++ init s ++ ") \"\" accusative ;"
                                  V2Q -> "mkV2Q (" ++ init s ++ ") \"\" accusative ;"
                                  V2A -> "mkV2A (" ++ init s ++ ") \"\" accusative ;"
                                  N2  -> "mkN2 ("  ++ init s ++ ") ;"
                                  N3  -> "mkN3 ("  ++ init s ++ ") ;"
                                  PN  -> "nounPN ("  ++ init s ++ ") ;"
                                  A2  -> "mkA2 ("  ++ init s ++ ") \"\" accusative ;"
                                  _  -> s

  sequence d >>= writeFile dictionaryRus . process
        where process :: [[(String, String, String, String)]] -> String
              process x = pre ++ (unlines          .
                                  map createLine   .
                                  map remDup .
                                  groupBy myElem .
                                  sortBy myOrder .
                                  concat) x
                                  ++ "\n}"

myOrder :: (String, String, String, String) ->
           (String, String, String, String) ->
           Ordering
myOrder (x1, _, _, _) (x2, _, _, _) | x1 < x2  = LT
                                    | x1 == x2 = EQ
                                    | x1 > x2  = GT

remDup :: [(String, String, String, String)] -> (String, String, String, String)
remDup xs = let wfss = map (\(_,_,_,wfs) -> wfs) xs
                (x1, x2, x3, _) = head xs
            in (x1, x2, x3, (intercalate " | " . filter (not.null) . (map (\x->if null x then "" else init x))) wfss)

createLine (absIdEng, _, _, wfs) =
               if wfs == ""
                 then ""
                 else "lin "
                      ++ absIdEng
                      ++ " = "
                      ++ wfs ++ " ;"

checkWNId :: AbsData -> PredRecord -> (AbsId, AbsId, Lemma) -> (AbsId, AbsId, Lemma, PoS)
checkWNId absData predRec (aId, aId', l) = case Map.lookup aId absData of
  Just absRec -> let pos = absPS absRec in
    if (absWNId absRec == predWNId predRec) &&
       (simplify pos  == predPS predRec)
    then (aId, aId', l, pos)
    else (aId, aId', l, UndefPoS)
  Nothing     -> (aId, aId', l, UndefPoS)


myElem (x1, _, _, _) (y1, _, _, _) = x1 == y1

-- | Returns MorphoAnalysis of a word
--lum :: WordForm -> IO [PGF2.MorphoAnalysis]
lum gr morpho conc = case Map.lookup conc (PGF2.languages gr) of
  Just eng -> return $ PGF2.lookupMorpho eng morpho
  Nothing  -> error $ conc ++ "is not found."


pre = "--# -path=.:alltenses \n\
\concrete DictionaryRus of Dictionary = CatRus ** \n\
  \open ParadigmsRus, Prelude, StructuralRus, MorphoRus in {\n\
\flags \n\
  \optimize=values ;\n\
  \coding=utf8 ;\n"

