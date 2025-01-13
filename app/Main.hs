module Main where

import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
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

wordProbability :: Word' -> Bow -> Float
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

spamBow :: IO Bow
spamBow = bowFromFolder "./data/train/spam/"

hamBow :: IO Bow
hamBow = bowFromFolder "./data/train/ham/"

wordToBow :: Word' -> Bow
wordToBow w = Bow $ M.fromList [(w, 1)]

textToBow :: T.Text -> Bow
textToBow = foldMap wordToBow . normalizeTextToWords

bowFromFile :: FilePath -> IO Bow
bowFromFile filePath = textToBow <$> T.readFile filePath

bowFromFolder :: FilePath -> IO Bow
bowFromFolder folderPath = do
  fileNames <- listDirectory folderPath
  bows <- mapM (bowFromFile . (folderPath <>)) fileNames
  pure $ fold bows

wordProbabilitySpam :: Word' -> IO Float
wordProbabilitySpam w = do
  pws <- wordProbability w <$> spamBow
  phs <- wordProbability w <$> hamBow
  let ps = pws + phs
  pure $ if ps == 0 then 0 else pws / (pws + phs)

textProabilitySpam :: T.Text -> IO Float
textProabilitySpam text = do
  let ws = normalizeTextToWords text
  ps <- mapM wordProbabilitySpam ws
  let ips = map (1.0 -) ps
  let pp = product ps
  pure $ pp / (pp + product ips)

main :: IO ()
main = putStrLn "Hello, Haskell!"
