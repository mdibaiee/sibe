{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
  import Numeric.Sibe
  import Numeric.Sibe.Word2Vec
  import Numeric.Sibe.Utils
  import Data.Default.Class
  import qualified Data.Vector.Storable as V
  import Data.List (sortBy)
  import Data.Function (on)
  import Numeric.LinearAlgebra
  import System.IO
  import System.Directory
  import Data.List.Split
  import Control.Exception (evaluate)
  import Debug.Trace
  import Data.Char
  import System.Random

  rf :: FilePath -> IO String
  rf p = do
    hs <- openFile p ReadMode
    hSetEncoding hs latin1
    content <- evaluate =<< hGetContents hs
    length content `seq` hClose hs
    return content

  main = do
    setStdGen (mkStdGen 100)
    sws <- lines <$> readFile "examples/stopwords"

    -- real data, takes a lot of time to train
    {-ds <- do-}
        {-files <- filter ((/= "xml") . take 1 . reverse) <$> listDirectory "examples/blogs-corpus/"-}
        {-contents <- mapM (rf . ("examples/blogs-corpus/" ++)) files-}

        {-let texts = map (unwords . splitOn "&nbsp;") contents-}
        {-let tags = ["<Blog>", "</Blog>", "<date>", "</date>", "<post>", "</post>", "&nbsp;"]-}
        {-return $ map cleanText $ removeWords (sws ++ tags) texts-}

    let ds = ["the king loves the queen", "the queen loves the king",
              "the dwarf hates the king", "the queen hates the dwarf",
              "the dwarf poisons the king", "the dwarf poisons the queen",
              "the man loves the woman", "the woman loves the man",
              "the thief hates the man", "the woman hates the thief",
              "the thief robs the man", "the thief robs the woman"]

    let session = def { learningRate = 0.1
                      , batchSize = 1
                      , epochs = 10000
                      , debug = True
                      } :: Session
        w2v = def { docs = ds
                  , dimensions = 30
                  , method = SkipGram
                  , window = 2
                  , w2vDrawChart = True
                  , w2vChartName = "w2v.png"
                  } :: Word2Vec

    (computed, vocvec) <- word2vec w2v session

    return ()

  cleanText :: String -> String
  cleanText string = 
    let notag = unwords $ filter ((/= "<date>") . take 6) (words string)
        ws = unwords $ filter (`notElem` ["urlLink"]) (words notag)
        spacify = foldl (\acc x -> replace x ' ' acc) (trim ws) [',', '/', '-', '\n', '\r', '?', '.', '(', ')', '%', '$', '"', ';', ':', '!', '\'']
        nonumber = filter (not . isNumber) spacify
        lower = map toLower nonumber
    in unwords . words $ lower
    where
      trim = f . f
        where
          f = reverse . dropWhile isSpace
      replace needle replacement =
        map (\c -> if c == needle then replacement else c)

  removeWords :: [String] -> [String] -> [String]
  removeWords ws documents =
    map rm documents
    where
        rm text = 
          unwords $ filter (`notElem` ws) (words text)

