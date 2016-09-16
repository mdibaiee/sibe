{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
  import Sibe
  import Sibe.Word2Vec
  import Sibe.Utils
  import Data.Default.Class
  import qualified Data.Vector.Storable as V
  import Data.List (sortBy)
  import Data.Function (on)
  import Numeric.LinearAlgebra
  import System.IO
  import Data.List.Split

  main = do
    sws <- lines <$> readFile "examples/stopwords"
    {-ds <- do-}
        {-content <- readFile "examples/doc-classifier-data/data-reuters"-}
        {-let splitted = splitOn (replicate 10 '-' ++ "\n") content-}
            {-d = concatMap (tail . lines) (take 100 splitted)-}
        {-return $ removeWords sws d-}
    --let ds = ["I like deep learning", "I like NLP", "I enjoy flying"]
    let ds = ["the king loves the queen", "the queen loves the king",
              "the dwarf hates the king", "the queen hates the dwarf",
              "the dwarf poisons the king", "the dwarf poisons the queen"]

    let session = def { learningRate = 0.1
                      , batchSize = 1
                      , epochs = 100
                      , debug = True
                      } :: Session
        w2v = def { docs = ds
                  , dimensions = 50
                  , method = SkipGram
                  , window = 2
                  } :: Word2Vec


    (computed, vocvec) <- word2vec w2v session
    
    mapM_ (\(w, v) -> do
                    putStr $ w ++ ": "
                    let similarities = map (similarity v . snd) computed
                    let sorted = sortBy (compare `on` similarity v . snd) computed
                    print . take 2 . drop 1 . reverse $ map fst sorted
          ) computed

    return ()

  removeWords :: [String] -> [String] -> [String]
  removeWords ws documents =
    map (rm ws) documents
    where
        rm list text =
          unwords $ filter (`notElem` list) (words text)

