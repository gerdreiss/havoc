{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Vocabulary as V
import qualified Data.List          as L
import qualified Data.Text          as T
import           System.Environment

data Command = Command
  { name :: T.Text
  , args :: [T.Text]
  }

execute :: Command -> IO ()
execute command
  | name command == "init"   = initV $ args command
  | name command == "add"    = add $ args command    -- putStrLn "Yeni kelime ekleme..."
  | name command == "update" = update $ args command -- putStrLn "Bir kelimeyi güncelleme..."
  | name command == "get"    = get $ args command    -- putStrLn "Bir kelime için çevirileri alma..."
  | name command == "list"   = list                  -- putStrLn "Tüm kelimeleri listelemek..."
  | otherwise                = putStrLn "Tam olarak ne istiyorsun?..."
  
initV :: [T.Text] -> IO ()
initV []         = putStrLn "Adı unuttun mu?"
initV (name : _) = V.init name

add :: [T.Text] -> IO ()
add (w : ts) = V.add $ V.lexi w ts
add _        = return ()

update :: [T.Text] -> IO ()
update (w : ts) = V.update $ V.lexi w ts
update _        = return ()

get :: [T.Text] -> IO ()
get (w : _) = do
  lexi <- V.get w
  case lexi of
    Just l -> print l
    _      -> putStrLn "Bulunamadı"
get _ = putStrLn "Sözcüğü göndermeyi unuttun mu??"

list :: IO ()
list = do
  lexi <- V.list
  case lexi of
    [] -> putStrLn "Hiçbirşey Bulunamadı"
    _  -> mapM_ print lexi

main :: IO ()
main = do
  args <- getArgs
  case args of
    (x : xs) -> execute Command { name = T.pack x, args = map T.pack xs }
    _        -> execute Command { name = "ne?", args = mempty }
