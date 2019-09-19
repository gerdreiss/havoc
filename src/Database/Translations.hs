{-# LANGUAGE OverloadedStrings #-}

module Database.Translations where

import qualified Data.List                      as L
import           Data.Text
import           Database.Connections
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data Translation =
  Translation
    { id          :: Int
    , wordId      :: Int
    , translation :: Text
    }

instance Show Translation where
  show tr = mconcat [unpack $ translation tr]

instance FromRow Translation where
  fromRow = Translation <$> field
                        <*> field
                        <*> field

instance FromRow Int where
  fromRow = field

-- | check whether translations exist for the given word ID
existForWordId :: Int -> IO Bool
existForWordId wordId = queryWithConn "tr-en.db" existTranslations
  where
    existTranslations conn = executeSelect conn >>= \res -> return $ L.null res
    executeSelect conn = query conn selectByWordIdQuery (Only wordId) :: IO [Int]

-- | check whether translations exist for the given word
existForWord :: Text -> IO Bool
existForWord word = queryWithConn "tr-en.db" existTranslations
  where
    existTranslations conn = executeSelect conn >>= \res -> return $ L.null res
    executeSelect conn = query conn selectByWordQuery (Only word) :: IO [Int]

-- | find translations for the given word id
findTranslationsForWordId :: Int -> IO [Translation]
findTranslationsForWordId wordId = queryWithConn "tr-en.db" executeQuery
  where
    executeQuery conn = query conn selectByWordIdQuery (Only wordId) :: IO [Translation]

-- | find translations for the given word
findTranslationsForWord :: Text -> IO [Translation]
findTranslationsForWord word = queryWithConn "tr-en.db" executeQuery
  where
    executeQuery conn = query conn selectByWordQuery (Only word) :: IO [Translation]

-- | add new translation to the word with the given id
addTranslationForWordId :: Int -> Text -> IO ()
addTranslationForWordId wordId translation = executeWithConn "tr-en.db" executeInsert
  where
    executeInsert conn = execute conn insertQueryForWordId (wordId, translation)

-- | add new translation to the word with the given
addTranslationForWord :: Text -> Text -> IO ()
addTranslationForWord word translation = executeWithConn "tr-en.db" executeInsert
  where
    executeInsert conn = execute conn insertQueryForWord (word, translation)

-- | queries
-- |
select1ByWordIdQuery :: Query
select1ByWordIdQuery = "SELECT 1 FROM translations WHERE word_id = ?"

select1ByWordQuery :: Query
select1ByWordQuery = "SELECT 1 FROM translations t INNER JOIN words w ON t.word_id = w.id WHERE w.word = ?"

selectByWordIdQuery :: Query
selectByWordIdQuery = "SELECT * FROM translations WHERE word_id = ?"

selectByWordQuery :: Query
selectByWordQuery = "SELECT * FROM translations t INNER JOIN words w ON t.word_id = w.id WHERE w.word = ?"

insertQueryForWordId :: Query
insertQueryForWordId = "INSERT INTO translations (word_id, translation) VALUES (?,?)"

insertQueryForWord :: Query
insertQueryForWord = "INSERT INTO translations (word_id, translation) VALUES (SELECT id FROM words where word = ?, ?)"
