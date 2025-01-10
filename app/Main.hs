module Main where

import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory

newtype Bow = Bow
  {bowToMap :: M.Map T.Text Int}
  deriving (Show, Read)

wordToBow :: T.Text -> Bow
wordToBow w = Bow $ M.fromList [(w, 1)]

emptyBow :: Bow
emptyBow = Bow M.empty

instance Semigroup Bow where
  Bow bow1 <> Bow bow2 = Bow $ M.unionWith (+) bow1 bow2

instance Monoid Bow where
  mempty = emptyBow

wordsCount :: Bow -> Int
wordsCount (Bow bow) = sum $ map snd $ M.toList bow

wordProbability :: T.Text -> Bow -> Float
wordProbability w bow = fromIntegral n / fromIntegral (wordsCount bow)
 where
  n = fromMaybe 0 $ M.lookup w $ bowToMap bow

textToBow :: T.Text -> Bow
textToBow = foldMap wordToBow . T.words

bowFromFile :: FilePath -> IO Bow
bowFromFile filePath = textToBow <$> T.readFile filePath

bowFromFolder :: FilePath -> IO Bow
bowFromFolder folderPath = do
  fileNames <- listDirectory folderPath
  bows <- mapM (bowFromFile . (folderPath <>)) fileNames
  pure $ fold bows

spamBow :: IO Bow
spamBow = bowFromFolder "./data/train/spam/"

hamBow :: IO Bow
hamBow = bowFromFolder "./data/train/ham/"

main :: IO ()
main = putStrLn "Hello, Haskell!"
