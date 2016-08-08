module Sibe.NaiveBayes
  (Document(..),
   NB(..),
   train,
   run,
   ordNub,
   accuracy,
   precision,
   recall,
   fmeasure,
   mean,
   stdev,
   cleanText,
   cleanDocuments,
   ngram,
   removeWords,
   removeStopwords,
  )
  where
    import Data.List
    import Debug.Trace
    import qualified Data.Set as Set
    import Data.List.Split
    import Data.Maybe
    import Control.Arrow ((&&&))
    import Text.Regex.PCRE
    import Data.Char (isSpace, isNumber)
    import NLP.Stemmer

    type Class = Int;

    data Document = Document { text :: String
                             , c    :: Class
                             } deriving (Eq, Show, Read)

    data NB = NB { documents  :: [Document]
                 , classes    :: [(Class, Double)]
                 , vocabulary :: Int
                 , megadoc    :: String
                 , cd         :: [(Class, [Document])]
                 , cw         :: [(Class, [(String, Int)])]
                 , cgram      :: [(Class, [(String, Int)])]
                 } deriving (Eq, Show, Read)

    train :: [Document] -> [Class] -> NB
    train documents classes =
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

    ngram :: Int -> [Document] -> [Document]
    ngram n documents =
      map (\(Document text c) -> Document (helper text) c) documents
      where
        helper text =
          let ws = words text
              pairs = zip [0..] ws
              grams = map (\(i, w) -> concat . intersperse "_" $ w:((take (n - 1) . drop (i+1)) ws)) pairs
          in unwords ("<b>":grams)

    run :: String -> NB -> (Class, Double)
    run txt (NB documents classes vocabulary megadoc cd cw cgram) =
      let scores = map (score . fst) classes
          index = argmax scores
          m = maximum scores
          confidence = m / sum scores
      in (index, 0)
      where
        score c =
          let prior = snd (classes !! c)

          -- below is the formula according to Multinominal Naive Bayes, but it seems
          -- using a uniform prior probability seems to work better
          -- in prior * product (map (prob c) (words txt))

          in product (map (prob c) (words txt))

        prob c w =
          let tctM = find ((== w) . fst) (snd (cw !! c))
              tct  = if isJust tctM then (snd . fromJust) tctM else 0
              cvoc = sum $ map snd (snd (cw !! c))
              voc  = vocabulary
              gram = find ((==w) . last . splitOn "_" . fst) (snd (cgram !! c))
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

    cleanText :: String -> String
    cleanText string =
      let puncs = filter (`notElem` ['!', '"', '#', '$', '%', '(', ')', '.', '?']) (trim string)
          spacify = foldl (\acc x -> replace x ' ' acc) puncs [',', '/', '-', '\n', '\r']
          stemmed = unwords $ map (stem Porter) (words spacify)
          nonumber = filter (not . isNumber) stemmed
      in (unwords . words) nonumber
      where
        trim = f . f
          where
            f = reverse . dropWhile isSpace
        replace needle replacement =
          map (\c -> if c == needle then replacement else c)

    cleanDocuments :: [Document] -> [Document]
    cleanDocuments documents =
      let cleaned = map (\(Document text c) -> Document (cleanText text) c) documents
      in cleaned

    removeWords :: [String] -> [Document] -> [Document]
    removeWords ws documents =
      map (\(Document text c) -> Document (rm ws text) c) documents
      where
          rm list text =
            unwords $ filter (`notElem` list) (words text)

    removeStopwords :: Int -> [Document] -> [Document]
    removeStopwords i documents =
      let wc = wordCounts (concatDocs documents)
          wlist = sortBy (\(_, a) (_, b) -> b `compare` a) wc
          stopwords = map fst (take i wlist)
      in removeWords stopwords documents
      where
        vocabulary x = ordNub (words x)
        countWordInDoc d w = genericLength (filter (==w) d)
        wordCounts x =
          let voc = vocabulary x
          in zip voc $ map (countWordInDoc (words x)) voc

        concatDocs = concatMap (\(Document text _) -> text ++ " ")

    l :: (Show a) => a -> a
    l a = trace (show a) a

    ordNub :: (Ord a) => [a] -> [a]
    ordNub = go Set.empty
      where
        go _ [] = []
        go s (x:xs) = if x `Set.member` s then go s xs
                                          else x : go (Set.insert x s) xs

    accuracy :: [(Int, (Int, Double))] -> Double
    accuracy results =
      let pairs = map (\(a, b) -> (a, fst b)) results
          correct = filter (uncurry (==)) pairs
      in genericLength correct / genericLength results

    recall :: [(Int, (Int, Double))] -> Double
    recall results =
      let classes = ordNub (map fst results)
          s = sum (map rec classes) / genericLength classes
      in s
      where
        rec a =
          let t = genericLength $ filter (\(c, (r, _)) -> c == r && c == a) results
              y = genericLength $ filter (\(c, (r, _)) -> c == a) results
          in t / y

    precision :: [(Int, (Int, Double))] -> Double
    precision results =
      let classes = ordNub (map fst results)
          s = sum (map prec classes) / genericLength classes
      in s
      where
        prec a =
          let t = genericLength $ filter (\(c, (r, _)) -> c == r && c == a) results
              y = genericLength $ filter (\(c, (r, _)) -> r == a) results
          in
            if y == 0
              then 0
              else t / y

    fmeasure :: [(Int, (Int, Double))] -> Double
    fmeasure results =
      let r = recall results
          p = precision results
      in (2 * p * r) / (p + r)
