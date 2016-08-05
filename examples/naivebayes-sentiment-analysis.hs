{-# LANGUAGE BangPatterns #-}
module Main
  where
    -- import Sibe
    import Sibe.NaiveBayes
    import Text.Printf
    import Data.List
    import Data.Maybe
    import Debug.Trace
    import Data.List.Split
    import System.Directory
    import Control.DeepSeq
    import System.IO

    main = do
      putStr "Reading documents... "
      neg_documents <- createDocuments "examples/sentiment-analysis-data/train/neg/"
      pos_documents <- createDocuments "examples/sentiment-analysis-data/train/pos/"

      test_neg_documents  <- createDocuments "examples/sentiment-analysis-data/test/neg/"
      test_pos_documents <- createDocuments "examples/sentiment-analysis-data/test/pos/"
      putStrLn "done"

      let classes = [0..9] -- rating, from 0 to 9 (1 to 10)
          documents = neg_documents ++ pos_documents
          nb = train documents classes

          testDocuments = neg_documents ++ pos_documents

          results = map (\(Document text c) -> (c, run text nb)) testDocuments
          -- results = map (\(Document text c) -> (c, determine text nb intClasses documents)) devTestDocuments
      print results

      -- let showResults (c, r) = putStrLn (show (classes !! c) ++ " ~ " ++ show (classes !! r))
      -- mapM_ showResults results
      --
      -- putStrLn $ "Recall: " ++ show (recall results)
      -- putStrLn $ "Precision: " ++ show (precision results)
      -- putStrLn $ "F Measure: " ++ show (fmeasure results)
      -- putStrLn $ "Accuracy: " ++ show (accuracy results)

    createDocuments :: FilePath -> IO [Document]
    createDocuments path = do
      files <- drop 2 <$> getDirectoryContents path
      let ratings = map (subtract 1 . read . take 1 . last . splitOn "_") files :: [Int]
      contents <- mapM (forceReadFile . (path ++)) files
      return $ zipWith Document contents ratings

    forceReadFile :: FilePath -> IO String
    forceReadFile file = do
      handle <- openFile file ReadMode
      content <- hGetContents handle
      content `deepseq` hClose handle
      return content
