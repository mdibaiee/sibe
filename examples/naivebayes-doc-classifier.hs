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
    import Control.Monad (when)
    import System.Environment

    main = do
      args <- getArgs
      dataset <- readFile "examples/doc-classifier-data/data-reuters"
      test <- readFile "examples/doc-classifier-data/data-reuters-test"

      classes <- map (filter (/= ' ')) . lines <$> readFile "examples/doc-classifier-data/data-classes"
      sws <- lines <$> readFile "examples/stopwords"

      let verbose = or [elem "-v" args, elem "--verbose" args]
      when (not verbose) $ putStrLn "use --verbose to print more information"

      let intClasses = [0..length classes - 1]
          documents = cleanDocuments $ removeWords sws $ createDocuments classes dataset
          testDocuments = cleanDocuments $ createDocuments classes test
          devTestDocuments = take 30 testDocuments
          nb = train documents intClasses

          results = session testDocuments nb
          -- results = session devTestDocuments nb

      when verbose $ print (text $ head documents)

      let showResults (c, (r, confidence)) = putStrLn (classes !! c ++ " ~ " ++ classes !! r)
      when verbose $ mapM_ showResults results

      when verbose $
        putStrLn $ "The training data is imbalanced which causes the classifier to be biased towards\n"
                ++ "some classes, `earn` is an example, the class alone has around 90% accuracy while\n"
                ++ "the rest of classes have a much lower accuracy and it's commonly seen that most inputs\n"
                ++ "are incorrectly classified as `earn`.\n"

      let
        accuracies =
          let as = zip intClasses $ map (\c -> filter ((==c) . fst) results) intClasses
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
