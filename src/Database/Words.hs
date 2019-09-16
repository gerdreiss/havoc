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

exists :: Text -> IO Bool
exists lexi = queryWithConn "tr-en.db" existsWord
  where
    existsWord conn = executeSelect conn >>= \res -> return $ L.null res
    executeSelect conn = query conn "SELECT 1 FROM words WHERE word = ?" (Only lexi) :: IO [Int]

findWord :: Text -> IO (Maybe Lexi)
findWord lexi = queryWithConn "tr-en.db" executeQuery
  where
    executeQuery conn  = executeSelect conn >>= maybeHead
    executeSelect conn = query conn "SELECT * FROM words WHERE word = ?" (Only lexi) :: IO [Lexi]
    maybeHead []       = return Nothing
    maybeHead (x:_)    = return (Just x)

addWord :: Text -> Text -> IO ()
addWord lexi annotation = executeWithConn "tr-en.db" executeInsert
  where
    executeInsert conn = execute conn "INSERT INTO words (word, annotation) VALUES (?,?)" (lexi, annotation)
