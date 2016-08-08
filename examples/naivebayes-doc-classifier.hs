module Main
  where
    -- import Sibe
    import Sibe.NaiveBayes
    import Text.Printf
    import Data.List
    import Data.Maybe
    import Debug.Trace
    import Data.List.Split
    import Control.Arrow ((&&&))

    main = do
      dataset <- readFile "examples/doc-classifier-data/data-reuters"
      test <- readFile "examples/doc-classifier-data/data-reuters-test"

      classes <- map (filter (/= ' ')) . lines <$> readFile "examples/doc-classifier-data/data-classes"
      sws <- lines <$> readFile "examples/stopwords"

      let intClasses = [0..length classes - 1]
          documents = cleanDocuments $ removeWords sws $ createDocuments classes dataset
          testDocuments = cleanDocuments $ createDocuments classes test
          devTestDocuments = take 30 testDocuments
          nb = train documents intClasses

          results = map (\(Document text c) -> (c, run text nb)) testDocuments
          -- results = map (\(Document text c) -> (c, run text nb)) devTestDocuments

      print (text $ head documents)

      let showResults (c, (r, confidence)) = putStrLn (classes !! c ++ " ~ " ++ classes !! r)
      mapM_ showResults results

      putStrLn $ "Recall: " ++ show (recall results)
      putStrLn $ "Precision: " ++ show (precision results)
      putStrLn $ "F Measure: " ++ show (fmeasure results)
      putStrLn $ "Accuracy: " ++ show (accuracy results)

    createDocuments classes content =
      let splitted = splitOn (replicate 10 '-' ++ "\n") content
          pairs = map ((head . lines) &&& (unwords . tail . lines)) splitted
          documents = map (\(topic, text) -> Document text (fromJust $ elemIndex topic classes)) pairs
      in documents
