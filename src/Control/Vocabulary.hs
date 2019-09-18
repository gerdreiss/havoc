module Control.Vocabulary where

import qualified Data.Text      as T
import qualified Database.Words as W

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
get w = fromPersistentLexi <$> W.findWord w

-- attempts to add the given Lexi to the vocabulary
add :: Lexi -> IO ()
add w = W.addWord (word w) (annotation w)

-- attempts to update the vocabulary
update :: Lexi -> IO Bool
update _ = return False

fromPersistentLexi :: Maybe W.Lexi -> Maybe Lexi
fromPersistentLexi (Just pw) = Just $ Lexi (W.value pw) ([]) (W.annotation pw)
fromPersistentLexi _         = Nothing
