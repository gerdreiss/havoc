module Vocabulary where

import Data.Text

data Lexi = Lexi
  { word         :: Text
  , translations :: [Text]
  , annotation   :: Text
  }

-- checks whether the given word exists in the vocabulary
exists :: Text -> IO Bool
exists _ = return False

-- attempts to retrieve the given word from the vocabulary
get :: Text -> IO (Maybe Lexi)
get _ = return Nothing 

-- attempts to add the given Lexi to the vocabulary
add :: Lexi -> IO Bool
add _ = return False

-- attempts to update the vocabulary
update :: Lexi -> IO Bool
update _ = return False
