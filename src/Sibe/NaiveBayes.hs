module Sibe.NaiveBayes
  (Document(..),
   NB(..),
   initialize,
   run,
   session,
   accuracy,
   precision,
   recall,
   fmeasure,
   mean,
   stdev,
   cleanText,
   cleanDocuments,
   ngram,
   ngramText,
   removeWords,
   removeStopwords,
  )
  where
    import Sibe.Utils
    import Sibe.NLP
    import Data.List
    import Debug.Trace
    import qualified Data.Set as Set
    import Data.List.Split
    import Data.Maybe
    import Control.Arrow ((&&&))

    data NB = NB { documents  :: [Document]
                 , classes    :: [(Class, Double)]
                 , vocabulary :: Int
                 , megadoc    :: String
                 , cd         :: [(Class, [Document])]
                 , cw         :: [(Class, [(String, Int)])]
                 , cgram      :: [(Class, [(String, Int)])]
                 } deriving (Eq, Show, Read)

    initialize :: [Document] -> [Class] -> NB
    initialize documents classes =
      let megadoc = concatDocs documents
          vocabulary = genericLength ((ordNub . words) megadoc)
          -- (class, prior probability)
          cls = zip classes (map classPrior classes)

          -- (class, [document])
          cd = zip classes (map classDocs classes)

          -- (class, [(word, count)])
          cw = zip classes $ map classWordsCounts classes

          cgram = zip classes $ map classNGramCounts classes

      in NB { documents  = documents
            , classes    = cls
            , vocabulary = vocabulary
            , megadoc    = megadoc
            , cd         = cd
            , cw         = cw
            , cgram      = cgram
            }
      where
        concatDocs = concatMap (\(Document text _) -> text ++ " ")

        classDocs x = filter ((==x) . c) documents
        classMegadoc = concatMap (\(Document text _) -> text ++ " ") . classDocs
        classWords = words . classMegadoc
        classNGram = concatMap (\(Document text _) -> text ++ " ") . ngram 2 . classDocs
        classNGramWords = words . classNGram
        classVocabulary = ordNub . classWords
        classPrior x = genericLength (classDocs x) / genericLength documents
        countWordInDoc d w = genericLength (filter (==w) d)
        wordsCount ws voc =
          zip voc $ map (countWordInDoc ws) voc
        classWordsCounts x = wordsCount (classWords x) (classVocabulary x)
        classNGramCounts x = wordsCount (classNGramWords x) (ordNub $ classNGramWords x)

    session :: [Document] -> NB -> [(Class, (Class, Double))]
    session docs nb =
      let results = map (\(Document text c) -> (c, run text nb)) docs
      in results

    run :: String -> NB -> (Class, Double)
    run txt (NB documents classes vocabulary megadoc cd cw cgram) =
      let scores = map (score . fst) classes
          index = argmax scores
          m = maximum scores
      in (fst (classes !! index), m)
      where
        score c =
          let prior = snd (fromJust $ find ((==c) . fst) classes)

          -- below is the formula according to Multinominal Naive Bayes, but it seems
          -- using a uniform prior probability seems to work better when working with imbalanced
          -- training datasets, instead, we help rare classes get higher scores using
          -- alpha = (1 - prior * ALPHA), we use ALPHA = 1 here
          -- in prior * product (map (prob c) (words txt))

              alpha = 1 - prior

          in alpha * product (map (prob c) (words txt))

        prob c w =
          let fcw  = fromJust $ find ((==c) . fst) cw
              fcg  = fromJust $ find ((==c) . fst) cgram
              tctM = find ((== w) . fst) (snd fcw)
              tct  = if isJust tctM then (snd . fromJust) tctM else 0
              cvoc = sum $ map snd (snd fcw)
              voc  = vocabulary
              gram = find ((==w) . last . splitOn "_" . fst) (snd fcg)
              pg   = if isJust gram then (snd . fromJust) gram else 0
          -- in realToFrac (tct * pg + 1) / realToFrac (cvoc + voc) -- uncomment to enable ngrams
          in realToFrac (tct + 1) / realToFrac (cvoc + voc)

    argmax :: (Ord a) => [a] -> Int
    argmax x = fst $ maximumBy (\(_, a) (_, b) -> a `compare` b) (zip [0..] x)

    mean :: [Double] -> Double
    mean x = sum x / genericLength x

    stdev :: [Double] -> Double
    stdev x =
      let avg = mean x
          variance = sum (map ((^2) . subtract avg) x) / (genericLength x - 1)
      in sqrt variance

    l :: (Show a) => a -> a
    l a = trace (show a) a
