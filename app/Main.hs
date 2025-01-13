{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.Directory
import Text.Printf

newtype Word' = Word' T.Text
  deriving (Show, Read, Ord, Eq)

mkWord :: T.Text -> Word'
mkWord = Word' . T.toUpper

type Freq = Int
type Prob = Double

newtype Bow a = Bow
  {bowToMap :: M.Map Word' a}
  deriving (Show, Read)

emptyBow :: Bow Freq
emptyBow = Bow M.empty

summaryBow :: Bow Freq -> IO ()
summaryBow (Bow bow) = do
  forM_
    ( sortBy (compare `on` snd) $
        M.toList bow
    )
    $ \(w, f) -> printf "%s -> %d\n" (wordToText w) f

instance Semigroup (Bow Freq) where
  Bow bow1 <> Bow bow2 = Bow $ M.unionWith (+) bow1 bow2

instance Monoid (Bow Freq) where
  mempty = emptyBow

wordToText :: Word' -> T.Text
wordToText (Word' t) = t

wordsCount :: Bow Freq -> Int
wordsCount (Bow bow) = sum $ map snd $ M.toList bow

wordProbability :: Word' -> Bow Prob -> Prob
wordProbability w bow = fromMaybe 0 $ M.lookup w $ bowToMap bow

freqToProb :: Bow Freq -> Bow Prob
freqToProb bow =
  Bow $
    M.map (\x -> fromIntegral x / n) $
      bowToMap bow
 where
  n = fromIntegral $ wordsCount bow

normalizeTextToWords :: T.Text -> [Word']
normalizeTextToWords =
  map mkWord
    . T.words
    . T.map
      ( \w -> if isAlphaNum w then w else ' '
      )

wordToBow :: Word' -> Bow Freq
wordToBow w = Bow $ M.fromList [(w, 1)]

textToBow :: T.Text -> Bow Freq
textToBow = foldMap wordToBow . normalizeTextToWords

seenWord :: SpamModel -> Word' -> Bool
seenWord (SpamModel (Bow spamBow') (Bow hamBow')) w = isJust sm || isJust hm
 where
  sm = M.lookup w spamBow'
  hm = M.lookup w hamBow'

readFileIfPossible :: FilePath -> IO (Maybe T.Text)
readFileIfPossible filePath = do
  bytes <- B.readFile filePath
  return $ case T.decodeUtf8' bytes of
    Right text -> Just text
    Left _ -> Nothing

bowFromFile :: FilePath -> IO (Maybe (Bow Freq))
bowFromFile filePath = do
  contents <- readFileIfPossible filePath
  return $ textToBow <$> contents

bowFromFolder :: FilePath -> IO (Bow Freq)
bowFromFolder folderPath = do
  fileNames <- listDirectory folderPath
  bows <- mapM (bowFromFile . (folderPath <>)) fileNames
  return $ fold $ catMaybes bows

data SpamModel = SpamModel
  { spamBow :: !(Bow Prob)
  , hamBow :: !(Bow Prob)
  }

spamModel :: IO SpamModel
spamModel = do
  spam <- loadBowCSV "./data/spam.csv"
  ham <- loadBowCSV "./data/ham.csv"
  pure $ SpamModel spam ham

wordProbabilitySpam :: SpamModel -> Word' -> Maybe Double
wordProbabilitySpam sm@(SpamModel spamBow' hamBow') w
  | seenWord sm w =
      let
        pws = wordProbability w spamBow'
        phs = wordProbability w hamBow'
        ps = pws + phs
       in
        Just (pws / ps)
  | otherwise = Nothing

wordProbabilityHam :: SpamModel -> Word' -> Maybe Double
wordProbabilityHam sm@(SpamModel spamBow' hamBow') w
  | seenWord sm w =
      let
        pws = wordProbability w spamBow'
        phs = wordProbability w hamBow'
        ps = pws + phs
       in
        Just (phs / ps)
  | otherwise = Nothing

textProabilitySpam :: SpamModel -> T.Text -> Double
textProabilitySpam sm text = pp / (pp + product ips)
 where
  ws = normalizeTextToWords text
  ps = mapMaybe (wordProbabilitySpam sm) ws
  ips = map (1.0 -) ps
  pp = product ps

classifyText :: SpamModel -> T.Text -> Double
classifyText = textProabilitySpam

classifyFile :: SpamModel -> FilePath -> IO Double
classifyFile sm filePath = classifyText sm <$> T.readFile filePath

classifyFolder :: SpamModel -> FilePath -> IO ()
classifyFolder sm folderPath = do
  fileNames <- listDirectory folderPath
  forM_ fileNames $ \fileName -> do
    let filePath = folderPath <> "/" <> fileName
    stats <- classifyFile sm filePath
    printf "%s -> %s\n" filePath (show stats)

dumpBowCsv :: (Show a) => Bow a -> FilePath -> IO ()
dumpBowCsv bow filePath =
  writeFile filePath $
    unlines $
      map (\(Word' word, value) -> printf "%s,%s" word (show value)) $
        M.toList $
          bowToMap bow

loadBowCSV :: (Read a) => FilePath -> IO (Bow a)
loadBowCSV filePath =
  Bow
    . M.fromList
    . map
      ( \line ->
          let [word, value] = T.splitOn "," line
           in (Word' word, read $ T.unpack value)
      )
    . T.lines
    <$> T.readFile filePath

train :: IO ()
train = do
  putStrLn "Training HAM:"
  ham <- freqToProb <$> bowFromFolder "./data/train/ham/"
  dumpBowCsv ham "./data/ham.csv"
  putStrLn "Training SPAM:"
  spam <- freqToProb <$> bowFromFolder "./data/train/spam/"
  dumpBowCsv spam "./data/spam.csv"

main :: IO ()
main = do
  sm <- spamModel
  putStrLn "Absolute HAM:"
  classifyFolder sm "./data/validate/ham/"
  putStrLn ""
  putStrLn "Absolute SPAM:"
  classifyFolder sm "./data/validate/spam/"
