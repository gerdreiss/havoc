{-# LANGUAGE OverloadedStrings #-}

module Control.Vocabulary where

import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text             as T
import qualified Database.DDL          as DDL
import qualified Database.Translations as X
import qualified Database.Words        as W

data Lexi = Lexi
  { word         :: T.Text
  , translations :: [T.Text]
  , annotation   :: T.Text
  }

instance Show Lexi where
  show lexi = mconcat
    [ T.unpack $ word lexi
    , " | "
    , T.unpack $ annotation lexi
    , " | "
    , concatMap T.unpack . translations $ lexi
    ]

lexi :: T.Text -> [T.Text] -> Lexi
lexi w ts = Lexi { word = w, translations = ts, annotation = "" }

-- checks whether the given word exists in the vocabulary
exists :: T.Text -> T.Text -> IO Bool
exists = W.exists

-- attempts to retrieve the given word from the vocabulary incl. translations
get :: T.Text -> T.Text -> IO (Maybe Lexi)
get db w = do
  maybeLexi <- W.findWord db w
  guard (isJust maybeLexi)
  ts <- X.findTranslationsForWordId db (W.id . fromJust $ maybeLexi)
  return $ fmap (withTranslations ts) maybeLexi
 where
  withTranslations txs pw = Lexi { word         = W.value pw
                                 , translations = fmap X.translation txs
                                 , annotation   = W.annotation pw
                                 }

-- attempts to retrıeve all words from the vocabulary w/o translations
list :: T.Text -> IO [T.Text]
list db = fmap W.value <$> W.list db

-- attempts to add the given Lexi to the vocabulary
add :: T.Text -> Lexi -> IO ()
add db w = do
  W.addWord db (word w) (annotation w)
  maybeLexi <- W.findWord db (word w)
  guard (isJust maybeLexi)
  traverse_ (X.addTranslationForWordId db . W.id . fromJust $ maybeLexi) (translations w)

-- attempts to update the vocabulary
update :: Lexi -> IO ()
update _ = pure ()

-- / initializes the database
init :: T.Text -> IO ()
init = DDL.init
