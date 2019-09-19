module Control.Vocabulary where

import           Data.Foldable
import qualified Data.Text             as T
import qualified Database.Translations as Txs
import qualified Database.Words        as W

data Lexi =
  Lexi
    { word         :: T.Text
    , translations :: [T.Text]
    , annotation   :: T.Text
    }

-- checks whether the given word exists in the vocabulary
exists :: T.Text -> IO Bool
exists = W.exists

-- attempts to retrieve the given word from the vocabulary incl. translations
get :: T.Text -> IO (Maybe Lexi)
get w = do
  maybePW <- W.findWord w
  txs <- Txs.findTranslationsForWord w
  return $ fmap (translations txs) maybePW
  where
    translations txs pw = Lexi { word = W.value pw
                               , translations = map Txs.translation txs
                               , annotation = W.annotation pw
                               }

-- attempts to add the given Lexi to the vocabulary
add :: Lexi -> IO ()
add w = do
  W.addWord (word w) (annotation w)
  traverse_ (Txs.addTranslationForWord (word w)) (translations w)

-- attempts to update the vocabulary
update :: Lexi -> IO Bool
update _ = return False
