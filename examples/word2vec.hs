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
    {-ds <- do
        content <- readFile "examples/doc-classifier-data/data-reuters"
        let splitted = splitOn (replicate 10 '-' ++ "\n") content
            d = concatMap (tail . lines) (take 100 splitted)
        return d-}
    let ds = ["I like deep learning", "I like NLP", "I enjoy flying"]

    let session = def { learningRate = 0.8
                      , batchSize = 10
                      , epochs = 1000
                      } :: Session
        w2v = def { docs = ds }:: Word2Vec


    r <- word2vec w2v session
    {-print r-}
    return ()
