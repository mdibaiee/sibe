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
    import Control.Monad (when, unless)
    import Data.Function (on)
    import System.Environment

    main = do
      args <- getArgs
      dataset <- readFile "examples/doc-classifier-data/data-reuters"
      test <- readFile "examples/doc-classifier-data/data-reuters-test"

      classes <- map (filter (/= ' ')) . lines <$> readFile "examples/doc-classifier-data/data-classes"
      sws <- lines <$> readFile "examples/stopwords"

      let verbose = elem "-v" args || elem "--verbose" args
          topten  = elem "-10" args || elem "--top-ten" args
      unless verbose $ putStrLn "use --verbose to print more information"

      let intClasses = [0..length classes - 1]
          documents = cleanDocuments . removeWords sws $ createDocuments classes dataset
          testDocuments = cleanDocuments $ createDocuments classes test

          nb = train documents intClasses

          -- top-ten
          topClasses = take 10 . reverse $ sortBy (compare `on` (length . snd)) (cd nb)
          filtered = map (\(c, ds) -> (c, take 100 ds)) topClasses
          filteredClasses = map fst filtered
          ttDocs = concatMap snd filtered
          ttNB = train ttDocs filteredClasses

          ttTestDocuments = filter ((`elem` filteredClasses) . c) . cleanDocuments $ createDocuments classes test

          ttResults = session ttTestDocuments ttNB
          normalResults = session testDocuments nb
          results = if topten then ttResults else normalResults

          iClasses = if topten then filteredClasses else intClasses
          -- results = session devTestDocuments nb

      when verbose . putStrLn $ "# Example of cleaned document:\n" ++ (show . text $ head documents)

      let showResults (c, (r, confidence)) = putStrLn (classes !! c ++ " ~ " ++ classes !! r)
      when verbose $ mapM_ showResults results

      when (verbose && not topten) .
        putStrLn $ "The training data is imbalanced which causes the classifier to be biased towards\n"
                ++ "some classes, `earn` is an example, the class alone has around 90% accuracy while\n"
                ++ "the rest of classes have a much lower accuracy and it's commonly seen that most inputs\n"
                ++ "are incorrectly classified as `earn`.\n"
                ++ "Try running with --top-ten to classify top 10 classes by using evenly split documents\n"

      let
        accuracies =
          let as = zip iClasses $ map (\c -> filter ((==c) . fst) results) iClasses
              av = filter (not . null . snd) as
              calculated = map (fst &&& accuracy . snd) av
          in sortBy (\(_, a) (_, b) -> b `compare` a) calculated

      when verbose $
        mapM_ (\(c, a) -> putStrLn $ "Accuracy(" ++ classes !! c ++ ") = " ++ show a) accuracies

      putStrLn $ "\nAverages: "
      putStrLn $ "Recall = " ++ show (recall results)
      putStrLn $ "Precision = " ++ show (precision results)
      putStrLn $ "F Measure = " ++ show (fmeasure results)
      putStrLn $ "Accuracy = " ++ show (accuracy results)

    createDocuments classes content =
      let splitted = splitOn (replicate 10 '-' ++ "\n") content
          pairs = map ((head . lines) &&& (unwords . tail . lines)) splitted
          documents = map (\(topic, text) -> Document text (fromJust $ elemIndex topic classes)) pairs
      in documents
