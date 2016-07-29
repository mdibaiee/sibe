module Main
  where
    import Sibe
    import Sibe.NaiveBayes
    import Text.Printf
    import Data.List
    import Data.Maybe
    import Debug.Trace

    main = do
      dataset <- readFile "examples/naivebayes-doc-classifier/data-reuters"
      test <- readFile "examples/naivebayes-doc-classifier/data-reuters-test"

      classes <- map (filter (/= ' ')) . lines <$> readFile "examples/naivebayes-doc-classifier/data-classes"

      let intClasses = [0..length classes - 1]
          documents = createDocuments classes dataset
          testDocuments = createDocuments classes test
          nb = initialize documents

      let testResults (Document text c) =
            let r = determine text nb intClasses documents
            in trace (classes !! c ++ " ~ " ++ classes !! r) c == r

      let results = map testResults testDocuments

      putStr "Accuracy: "
      putStr . show . round $ (genericLength (filter (==True) results) / genericLength results) * 100
      putStrLn "%"
