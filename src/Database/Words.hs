{-# LANGUAGE OverloadedStrings #-}

module Database.Words where

import qualified Data.List                      as L
import           Data.Text
import           Database.Connections
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data Lexi =
  Lexi
    { id         :: Int
    , value      :: Text
    , annotation :: Text
    }

instance Show Lexi where
  show lexi = mconcat
    [ unpack $ value lexi
    , " | "
    , unpack $ annotation lexi
    ]

instance FromRow Lexi where
  fromRow = Lexi <$> field
                 <*> field
                 <*> field

instance FromRow Int where
  fromRow = field

-- | checks whether the given word exists
exists :: Text -> IO Bool
exists lexi = queryWithConn "tr-en.db" existsWord
 where
  existsWord conn = executeSelect conn >>= \res -> return $ L.null res
  executeSelect conn = query conn select1ByWordQuery (Only lexi) :: IO [Int]

-- | retrieve all words
list :: IO [Lexi]
list = queryWithConn "tr-en.db" executeSelect
  where
    executeSelect conn = query_ conn selectAllQuery :: IO [Lexi]

-- | find the database entry for the given word
findWord :: Text -> IO (Maybe Lexi)
findWord lexi = queryWithConn "tr-en.db" executeQuery
 where
  executeQuery conn  = executeSelect conn >>= maybeHead
  executeSelect conn = query conn selectByWordQuery (Only lexi) :: IO [Lexi]
  maybeHead []       = return Nothing
  maybeHead (x : _)  = return (Just x)

-- | add a new word to the database
addWord :: Text -> Text -> IO ()
addWord lexi annotation = executeWithConn "tr-en.db" executeInsert
  where
    executeInsert conn = execute conn insertWordQuery (toRow (lexi, annotation))

-- | query constants
select1ByWordQuery :: Query
select1ByWordQuery = "SELECT 1 FROM words WHERE word = ?"

selectAllQuery :: Query
selectAllQuery = "SELECT * FROM words"

selectByWordQuery :: Query
selectByWordQuery = "SELECT * FROM words WHERE word = ?"

insertWordQuery :: Query
insertWordQuery = "INSERT INTO words (word, annotation) VALUES (?,?)"
