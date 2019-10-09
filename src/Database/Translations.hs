{-# LANGUAGE OverloadedStrings #-}

module Database.Translations where

import qualified Data.List                     as L
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
existForWordId :: Text -> Int -> IO Bool
existForWordId db wordId = queryWithConn filename existTranslations
  where
    filename = databaseFilename db
    existTranslations conn = executeSelect conn >>= \res -> return $ L.null res
    executeSelect conn = query conn selectByWordIdQuery (Only wordId) :: IO [Int]

-- | check whether translations exist for the given word
existForWord :: Text -> Text -> IO Bool
existForWord db word = queryWithConn filename existTranslations
  where
    filename = databaseFilename db
    existTranslations conn = executeSelect conn >>= \res -> return $ L.null res
    executeSelect conn = query conn selectByWordQuery (Only word) :: IO [Int]

-- | find translations for the given word id
findTranslationsForWordId :: Text -> Int -> IO [Translation]
findTranslationsForWordId db wordId = queryWithConn filename executeQuery
  where
    filename = databaseFilename db
    executeQuery conn = query conn selectByWordIdQuery (Only wordId) :: IO [Translation]

-- | find translations for the given word
findTranslationsForWord :: Text -> Text -> IO [Translation]
findTranslationsForWord db word = queryWithConn filename executeQuery
  where
    filename = databaseFilename db
    executeQuery conn = query conn selectByWordQuery (Only word) :: IO [Translation]

-- | add new translation to the word with the given id
addTranslationForWordId :: Text -> Int -> Text -> IO ()
addTranslationForWordId db wordId translation = executeWithConn filename executeInsert
  where
    filename = databaseFilename db
    executeInsert conn = execute conn insertQueryForWordId (toRow (wordId, translation))

-- | add new translation to the word with the given
addTranslationForWord :: Text -> Text -> Text -> IO ()
addTranslationForWord db word translation = executeWithConn filename executeInsert
  where
    filename = databaseFilename db
    executeInsert conn = execute conn insertQueryForWord (toRow (word, translation))

-- | queries
-- |
select1ByWordIdQuery :: Query
select1ByWordIdQuery = "SELECT 1 FROM translations WHERE word_id = ?"

select1ByWordQuery :: Query
select1ByWordQuery =
  "SELECT 1 FROM translations t INNER JOIN words w ON t.word_id = w.id WHERE w.word = ?"

selectByWordIdQuery :: Query
selectByWordIdQuery = "SELECT * FROM translations WHERE word_id = ?"

selectByWordQuery :: Query
selectByWordQuery =
  "SELECT * FROM translations t INNER JOIN words w ON t.word_id = w.id WHERE w.word = ?"

insertQueryForWordId :: Query
insertQueryForWordId =
  "INSERT INTO translations (word_id, translation) VALUES (?,?)"

insertQueryForWord :: Query
insertQueryForWord =
  "INSERT INTO translations (word_id, translation) VALUES (SELECT id FROM words where word = ?, ?)"
