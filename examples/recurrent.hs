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
  import qualified Data.ByteString.Lazy.Char8 as BSL

  main = do
    texts <- lines <$> readFile "examples/reddit.csv"
    let (vocabulary, indexes) = processData texts

    let settings = def { wordD = length vocabulary }
        r = randomRecurrent 0 settings

    let x0 = reverse . drop 1 . reverse $ indexes !! 0
        y0 = drop 1 $ indexes !! 0

    print $ x0
    print $ y0

    let xs = map (reverse . drop 1 . reverse) indexes
        ys = map (drop 1) indexes

    let tov = fromList . map fromIntegral
    let vys = map tov ys

    let newr = sgd r (take 1 xs) (take 1 vys) 0.005 1

    saveRecurrent "recurrent.trained" (show newr) 512
    --writeFile "recurrent.trained" (show newr)

    let newpredicted = predict newr x0
    print $ y0
    print $ newpredicted

    print $ loss (tov y0) (tov newpredicted)

    {-let (dU, dV, dW) = backprop r x0 (fromList $ map fromIntegral y0)-}
    {-print $ seq u "u"-}
    {-print $ seq v "v"-}
    {-print $ seq w "w"-}

    --print $ dW
    print "done"

  saveRecurrent :: FilePath -> String -> Int -> IO ()
  saveRecurrent path str chunkSize = do
    handle <- openFile path AppendMode
    hSetBuffering handle NoBuffering
    loop handle str
    hClose handle
    where
      loop _ [] = return ()
      loop handle s = do
        hPutStr handle $ take chunkSize s
        hFlush handle
        putStr $ take chunkSize s
        loop handle $ drop chunkSize s
    
