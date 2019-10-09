{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Vocabulary as V
import qualified Data.List          as L
import qualified Data.Text          as T
import           System.Environment

data Command = Command
  { name     :: T.Text
  , targetDb :: T.Text
  , args     :: [T.Text]
  }

execute :: Command -> IO ()
execute command
  | name command == "help"   = printHelp
  | name command == "init"   = initV (targetDb command)
  | name command == "add"    = add (targetDb command) (args command)    -- putStrLn "Yeni kelime ekleme..."
  | name command == "update" = update (targetDb command) (args command) -- putStrLn "Bir kelimeyi güncelleme..."
  | name command == "get"    = get (targetDb command) (args command)    -- putStrLn "Bir kelime için çevirileri alma..."
  | name command == "list"   = list (targetDb command)                  -- putStrLn "Tüm kelimeleri listelemek..."
  | otherwise                = putStrLn "Tam olarak ne istiyorsun?..."

printHelp :: IO ()
printHelp = putStrLn "TODO"

initV :: T.Text -> IO ()
initV name
  | T.empty == name = putStrLn "Adı unuttun mu?"
  | otherwise       = V.init name

add :: T.Text -> [T.Text] -> IO ()
add db (w : ts) = V.add db (V.lexi w ts)
add _ _         = return ()

update :: T.Text -> [T.Text] -> IO ()
update db (w : ts) = V.update $ V.lexi w ts
update _ _         = return ()

get :: T.Text -> [T.Text] -> IO ()
get db (w : _) = do
  lexi <- V.get db w
  case lexi of
    Just l -> print l
    _      -> putStrLn "Bulunamadı"
get _ _ = putStrLn "Sözcüğü göndermeyi unuttun mu??"

list :: T.Text -> IO ()
list db = do
  lexi <- V.list db
  case lexi of
    [] -> putStrLn "Hiçbirşey Bulunamadı"
    _  -> mapM_ print lexi

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("help" : _)   -> execute Command { name = "help"   , targetDb = T.empty  , args = mempty }
    (nm : db : xs) -> execute Command { name = T.pack nm, targetDb = T.pack db, args = fmap T.pack xs }
    _              -> execute Command { name = "ne?"    , targetDb = T.empty  , args = mempty }
