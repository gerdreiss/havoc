module Control.Vocabulary where

import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text             as Tx
import qualified Database.Translations as Ts
import qualified Database.Words        as W

data Lexi =
  Lexi
    { word         :: Tx.Text
    , translations :: [Tx.Text]
    , annotation   :: Tx.Text
    }

-- checks whether the given word exists in the vocabulary
exists :: Tx.Text -> IO Bool
exists = W.exists

-- attempts to retrieve the given word from the vocabulary incl. translations
get :: Tx.Text -> IO (Maybe Lexi)
get w = do
  maybeLexi <- W.findWord w
  guard (isJust maybeLexi)
  ts <- Ts.findTranslationsForWord w
  return $ fmap (withTranslations ts) maybeLexi
  where
    withTranslations txs pw =
      Lexi { word = W.value pw
           , translations = map Ts.translation txs
           , annotation = W.annotation pw
           }

-- attempts to add the given Lexi to the vocabulary
add :: Lexi -> IO ()
add w = W.addWord (word w) (annotation w) >>
        traverse_ (Ts.addTranslationForWord (word w)) (translations w)

-- attempts to update the vocabulary
update :: Lexi -> IO Bool
update _ = pure False
