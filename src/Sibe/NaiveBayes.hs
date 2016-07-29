module Sibe.NaiveBayes
  (Document(..),
   NB(..),
   createDocuments,
   initialize,
   calculate,
   determine
  )
  where
    import Data.List
    import Debug.Trace
    import qualified Data.Set as Set
    import Data.List.Split
    import Data.Maybe
    type Class = Int

    data Document = Document { text :: String
                             , c    :: Class
                             } deriving (Eq, Show, Read)

    data NB = NB { vocabulary :: Double
                 , megadoc    :: String
                 }

    initialize :: [Document] -> NB
    initialize documents =
      let megadoc = concatMap (\(Document text _) -> text ++ " ") documents
          vocabulary = genericLength ((ordNub . words) megadoc)
      in NB vocabulary megadoc

    determine :: String -> NB -> [Class] -> [Document] -> Class
    determine text nb classes documents =
      let scores = zip [0..] (map (\cls -> calculate text nb cls documents) classes)
          m = maximumBy (\(i0, c0) (i1, c1) -> c0 `compare` c1) scores
      in fst m

    calculate :: String -> NB -> Class -> [Document] -> Double
    calculate text (NB vocabulary megadoc) cls documents =
      let docs = filter (\(Document text c) -> c == cls) documents
          texts = map (\(Document text _) -> text ++ " ") docs
          classText = concat texts
          classWords = words classText
          c = genericLength classWords
          pc = genericLength docs / genericLength documents
      in pc * product (map (cword classWords c) (words text))
      where
        cword classWords c word =
          let wc = genericLength (filter (==word) classWords)
          in (wc + 1) / (c + vocabulary)

    createDocuments classes content =
      let splitted = splitOn (replicate 10 '-' ++ "\n") content
          pairs = map (\a -> ((head . lines) a, (concat . tail . lines) a)) splitted
          documents = map (\(topic, text) -> Document text (fromJust $ elemIndex topic classes)) pairs
      in documents

    l :: (Show a) => a -> a
    l a = trace (show a) a

    ordNub :: (Ord a) => [a] -> [a]
    ordNub = go Set.empty
      where
        go _ [] = []
        go s (x:xs) = if x `Set.member` s then go s xs
                                          else x : go (Set.insert x s) xs
