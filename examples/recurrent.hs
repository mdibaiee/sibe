{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
  import Numeric.LinearAlgebra
  import Numeric.Sibe.Recurrent
  import Numeric.Sibe.Utils
  import System.IO
  import Data.Default.Class
  import Data.List (genericLength)
  import qualified Data.ByteString.Lazy.Char8 as BL

  main = do
    texts <- lines <$> readFile "examples/reddit.csv"
    let (vocabulary, indexes) = processData texts

    let settings = def { wordD = length vocabulary }
        r = randomRecurrent 0 settings

    let x0 = reverse . drop 1 . reverse $ indexes !! 0
        y0 = drop 1 $ indexes !! 0

    let xs = map (reverse . drop 1 . reverse) indexes
        ys = map (drop 1) indexes

    let tov = fromList . map fromIntegral
    let vys = map tov ys

    let newr = sgd r (take 1 xs) (take 1 vys) 0.005 1

    let newpredicted = predict newr x0
    print $ y0
    print $ newpredicted

    print $ loss (tov y0) (tov newpredicted)

    print "done"

    saveRecurrent "recurrent.trained" (show newr) 512

  saveRecurrent :: FilePath -> String -> Int -> IO ()
  saveRecurrent path str chunkSize = do
    let b = BL.pack str
    withFile path AppendMode (process b)
    where
      process :: BL.ByteString -> Handle -> IO ()
      process b handle = do
        hSetBuffering handle NoBuffering
        loop handle b

      loop :: Handle -> BL.ByteString -> IO ()
      loop handle s 
        | s == BL.empty = return ()
        | otherwise = do
          let (current, next) = BL.splitAt (fromIntegral chunkSize) s
          BL.hPutStr handle current
          hFlush handle
          loop handle next
    
