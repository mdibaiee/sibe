module Numeric.Sibe.NLP
  (Class,
   Document(..),
   accuracy,
   recall,
   precision,
   fmeasure,
   cleanText,
   cleanDocuments,
   removeWords,
   removeStopwords,
   ngram,
   ngramText,
  )
  where
    import Numeric.Sibe.Utils
    import Data.List
    import Debug.Trace
    import Data.List.Split
    import Data.Maybe
    import Control.Arrow ((&&&))
    import Text.Regex.PCRE
    import Data.Char (isSpace, isNumber, toLower)
    import NLP.Stemmer
    import qualified Data.Set as Set

    type Class = Int;

    data Document = Document { text :: String
                             , c    :: Class
                             } deriving (Eq, Show, Read)


    cleanText :: String -> String
    cleanText string =
      let puncs = filter (`notElem` ['!', '"', '#', '$', '%', '(', ')', '.', '?']) (trim string)
          spacify = foldl (\acc x -> replace x ' ' acc) puncs [',', '/', '-', '\n', '\r']
          stemmed = unwords $ map (stem Porter) (words spacify)
          nonumber = filter (not . isNumber) stemmed
          lower = map toLower nonumber
      in (unwords . words) lower -- remove unnecessary spaces
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

    ngram :: Int -> [Document] -> [Document]
    ngram n documents =
      map (\(Document text c) -> Document (ngramText n text) c) documents

    ngramText :: Int -> String -> String
    ngramText n text =
      let ws = words text
          pairs = zip [0..] ws
          grams = map (\(i, w) -> concat . intersperse "_" $ w:((take (n - 1) . drop (i+1)) ws)) pairs
      in unwords ("<b>_":grams)
