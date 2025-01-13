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

newtype Bow = Bow
  {bowToMap :: M.Map Word' Int}
  deriving (Show, Read)

emptyBow :: Bow
emptyBow = Bow M.empty

summaryBow :: Bow -> IO ()
summaryBow (Bow bow) = do
  forM_
    ( sortBy (compare `on` snd) $
        M.toList bow
    )
    $ \(w, f) -> printf "%s -> %d\n" (wordToText w) f

instance Semigroup Bow where
  Bow bow1 <> Bow bow2 = Bow $ M.unionWith (+) bow1 bow2

instance Monoid Bow where
  mempty = emptyBow

wordToText :: Word' -> T.Text
wordToText (Word' t) = t

wordsCount :: Bow -> Int
wordsCount (Bow bow) = sum $ map snd $ M.toList bow

wordProbability :: Word' -> Bow -> Double
wordProbability w bow =
  fromIntegral n
    / fromIntegral (wordsCount bow)
 where
  n = fromMaybe 0 $ M.lookup w $ bowToMap bow

normalizeTextToWords :: T.Text -> [Word']
normalizeTextToWords =
  map mkWord
    . T.words
    . T.map
      ( \w -> if isAlphaNum w then w else ' '
      )

wordToBow :: Word' -> Bow
wordToBow w = Bow $ M.fromList [(w, 1)]

textToBow :: T.Text -> Bow
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

bowFromFile :: FilePath -> IO (Maybe Bow)
bowFromFile filePath = do
  contents <- readFileIfPossible filePath
  return $ textToBow <$> contents

bowFromFolder :: FilePath -> IO Bow
bowFromFolder folderPath = do
  fileNames <- listDirectory folderPath
  bows <- mapM (bowFromFile . (folderPath <>)) fileNames
  return $ fold $ catMaybes bows

data SpamModel = SpamModel
  { spamBow :: Bow
  , hamBow :: Bow
  }

spamModel :: IO SpamModel
spamModel = do
  spam <- bowFromFolder "./data/train/spam/"
  ham <- bowFromFolder "./data/train/ham/"
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

textProabilityHam :: SpamModel -> T.Text -> Double
textProabilityHam sm text = pp / (pp + product ips)
 where
  ws = normalizeTextToWords text
  ps = mapMaybe (wordProbabilityHam sm) ws
  ips = map (1.0 -) ps
  pp = product ps

classifyText :: SpamModel -> T.Text -> (Double, Double)
classifyText sm text = (textProabilitySpam sm text, textProabilityHam sm text)

classifyFile :: SpamModel -> FilePath -> IO (Double, Double)
classifyFile sm filePath = classifyText sm <$> T.readFile filePath

classifyFolder :: SpamModel -> FilePath -> IO ()
classifyFolder sm folderPath = do
  fileNames <- listDirectory folderPath
  forM_ fileNames $ \fileName -> do
    let filePath = folderPath <> "/" <> fileName
    stats <- classifyFile sm filePath
    printf "%s -> %s\n" filePath (show stats)

main :: IO ()
main = putStrLn "Hello, Haskell!"
