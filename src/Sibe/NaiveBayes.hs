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
  )
  where
    import Data.List
    import Debug.Trace
    import qualified Data.Set as Set
    import Data.List.Split
    import Data.Maybe
    import Control.Arrow ((&&&))
    import Text.Regex.PCRE
    import Data.Char (isSpace)
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
          cw = zip classes $ l (map classWordsCounts classes)

      in NB { documents  = documents
            , classes    = cls
            , vocabulary = vocabulary
            , megadoc    = megadoc
            , cd         = cd
            , cw         = cw
            }
      where
        concatDocs = concatMap (\(Document text _) -> text ++ " ")

        classDocs x = filter ((==x) . c) documents
        classMegadoc x = concatMap (\(Document text _) -> text ++ " ") (classDocs x)
        classWords x = words (classMegadoc x)
        classNGram n = ngram n . classMegadoc
        classVocabulary x = ordNub (classWords x)
        classPrior x = genericLength (classDocs x) / genericLength documents
        countWordInDoc d w = genericLength (filter (==w) d)
        classWordsCounts x =
          let voc = classVocabulary x
          in zip voc $ map (countWordInDoc (classWords x)) voc

    ngram :: Int -> String -> [String]
    ngram n text =
      let ws = words text
      in map (\(i, w) -> unwords $ w:((take (n - 1) . drop (i+1)) ws)) (zip [0..] ws)

    run :: String -> NB -> Class
    run text (NB documents classes vocabulary megadoc cd cw) =
      let scores = map (score . fst) classes
      in argmax scores
      where
        score c =
          let prior = snd (classes !! c)
          in prior * product (map (prob c) (words text))
        prob c w =
          let tctM = find ((==w) . fst) (snd (cw !! c))
              tct  = (snd . fromJust) tctM
              cvoc = (genericLength . snd) (cw !! c)
              voc  = vocabulary
          in
            if isJust tctM then
              realToFrac (tct + 1) / realToFrac (cvoc + voc)
            else
              1 / realToFrac (cvoc + voc)

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
      in stemmed
      where
        trim = f . f
          where
            f = reverse . dropWhile isSpace
        replace needle replacement =
          map (\c -> if c == needle then replacement else c)

    cleanDocuments :: [Document] -> [Document]
    cleanDocuments documents =
      let cleaned = map (\(Document text c) -> Document (cleanText text) c) documents
          wc = wordCounts (concatDocs cleaned)
          wlist = sortBy (\(_, a) (_, b) -> b `compare` a) wc
          stopwords = l $ map fst (take 30 wlist)
          wstopwords = map (\(Document text c) -> Document (removeWords stopwords text) c) cleaned
      in wstopwords
      where
        vocabulary x = ordNub (words x)
        countWordInDoc d w = genericLength (filter (==w) d)
        wordCounts x =
          let voc = vocabulary x
          in zip voc $ map (countWordInDoc (words x)) voc

        removeWords list text =
          unwords $ filter (`notElem` list) (words text)

        concatDocs = concatMap (\(Document text _) -> text ++ " ")

    l :: (Show a) => a -> a
    l a = trace (show a) a

    ordNub :: (Ord a) => [a] -> [a]
    ordNub = go Set.empty
      where
        go _ [] = []
        go s (x:xs) = if x `Set.member` s then go s xs
                                          else x : go (Set.insert x s) xs

    accuracy :: [(Int, Int)] -> Double
    accuracy results =
      let correct = filter (uncurry (==)) results
      in genericLength correct / genericLength results

    recall :: [(Int, Int)] -> Double
    recall results =
      let classes = ordNub (map fst results)
          s = sum (map rec classes) / genericLength classes
      in s
      where
        rec a =
          let t = genericLength $ filter (\(c, r) -> c == r && c == a) results
              y = genericLength $ filter (\(c, r) -> c == a) results
          in t / y

    precision :: [(Int, Int)] -> Double
    precision results =
      let classes = ordNub (map fst results)
          s = sum (map prec classes) / genericLength classes
      in s
      where
        prec a =
          let t = genericLength $ filter (\(c, r) -> c == r && c == a) results
              y = genericLength $ filter (\(c, r) -> r == a) results
          in
            if y == 0
              then 0
              else t / y

    fmeasure :: [(Int, Int)] -> Double
    fmeasure results =
      let r = recall results
          p = precision results
      in (2 * p * r) / (p + r)
