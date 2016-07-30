module Main
  where
    import Sibe
    import Sibe.NaiveBayes
    import Text.Printf
    import Data.List
    import Data.Maybe
    import Debug.Trace
    import Data.List.Split

    main = do
      dataset <- readFile "examples/doc-classifier-data/data-reuters"
      test <- readFile "examples/doc-classifier-data/data-reuters-test"

      classes <- map (filter (/= ' ')) . lines <$> readFile "examples/doc-classifier-data/data-classes"

      let intClasses = [0..length classes - 1]
          documents = createDocuments classes dataset
          testDocuments = createDocuments classes test
          devTestDocuments = take 20 testDocuments
          nb = initialize documents

          results = map (\(Document text c) -> (c, determine text nb intClasses documents)) testDocuments
          -- results = map (\(Document text c) -> (c, determine text nb intClasses documents)) devTestDocuments

      let showResults (c, r) = putStrLn (classes !! c ++ " ~ " ++ classes !! r)
      mapM_ showResults results

      putStrLn $ "Recall: " ++ show (recall results)
      putStrLn $ "Precision: " ++ show (precision results)
      putStrLn $ "F Measure: " ++ show (fmeasure (precision results) (recall results))
      putStrLn $ "Accuracy: " ++ show (accuracy results)
