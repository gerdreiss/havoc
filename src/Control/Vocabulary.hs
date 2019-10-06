{-# LANGUAGE OverloadedStrings #-}

module Control.Vocabulary where

import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text             as Tx
import qualified Database.DDL          as DDL
import qualified Database.Translations as Ts
import qualified Database.Words        as W

data Lexi = Lexi
  { word         :: Tx.Text
  , translations :: [Tx.Text]
  , annotation   :: Tx.Text
  }

instance Show Lexi where
  show lexi = mconcat
    [ Tx.unpack $ word lexi
    , " | "
    , Tx.unpack $ annotation lexi
    , "\n"
    , concatMap Tx.unpack . translations $ lexi
    ]

lexi :: Tx.Text -> [Tx.Text] -> Lexi
lexi w ts = Lexi { word = w, translations = ts, annotation = "" }

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
  withTranslations txs pw = Lexi { word         = W.value pw
                                 , translations = map Ts.translation txs
                                 , annotation   = W.annotation pw
                                 }

-- attempts to retrıeve all words from the vocabulary w/o translations
list :: IO [Tx.Text]
list = map W.value <$> W.list

-- attempts to add the given Lexi to the vocabulary
add :: Lexi -> IO ()
add w = do
  W.addWord (word w) (annotation w)
  maybeLexi <- W.findWord (word w)
  guard (isJust maybeLexi)
  traverse_ (Ts.addTranslationForWordId . W.id . fromJust $ maybeLexi) (translations w)

-- attempts to update the vocabulary
update :: Lexi -> IO ()
update _ = pure ()

-- / initializes the database
init :: Tx.Text -> IO ()
init = DDL.init
