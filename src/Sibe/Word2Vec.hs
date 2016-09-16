module Sibe.Word2Vec
  ( word2vec
  , Word2Vec (..)
  , W2VMethod (..)
  ) where
    import Sibe
    import Sibe.Utils
    import Debug.Trace
    import Data.Char
    import Data.Maybe
    import Data.List
    import Numeric.LinearAlgebra hiding (find)
    import qualified Data.Vector.Storable as V
    import Data.Default.Class
    import Data.Function (on)

    data W2VMethod = SkipGram | CBOW
    data Word2Vec = Word2Vec { docs :: [String]
                             , window :: Int
                             , dimensions :: Int
                             , method :: W2VMethod
                             }
    instance Default Word2Vec where
      def = Word2Vec { docs = []
                     , window = 2
                     }

    word2vec w2v session = do
      let s = session { training = trainingData
                      , network = randomNetwork 0 (-1, 1) v [(dimensions w2v, (id, one))] (v, (softmax, one))
                      }

      putStr "vocabulary size: "
      print v

      putStr "trainingData length: "
      print . length $ trainingData

      -- biases are not used in skipgram/cbow
      newses <- run (sgd . ignoreBiases) s


      -- export the hidden layer
      let (hidden@(Layer biases nodes _) :- _) = network newses
      -- run words through the hidden layer alone to get the word vector
      let computedVocVec = map (\(w, v) -> (w, runLayer' v hidden)) vocvec

      return (computedVocVec, vocvec)
      where
        -- clean documents
        ds = map cleanText (docs w2v)

        -- words of each document
        wd = map (words . (++ " ") . (map toLower)) ds

        -- all words together, used to generate the vocabulary
        ws = words (concatMap ((++ " ") . map toLower) ds)
        vocabulary = ordNub ws
        v = length vocabulary

        -- generate one-hot vectors for each word of vocabulary
        vocvec = zip vocabulary $ map (onehot v) [0..v - 1]

        -- training data: generate input and output pairs for each word and the words in it's window
        trainingData = concatMap (\wds -> concatMap (iter wds) $ zip [0..] wds) wd
          where
            iter wds (i, w) =
              let v = snd . fromJust . find ((==w) . fst) $ vocvec
                  before = take (window w2v) . drop (i - window w2v) $ wds
                  after = take (window w2v) . drop (i + 1) $ wds
                  ns 
                    | i == 0 = after
                    | i == length vocvec - 1 = before
                    | otherwise = before ++ after
                  vectorized = map (\w -> snd . fromJust $ find ((== w) . fst) vocvec) ns
                  new = foldl1 (+) vectorized
              in
                case method w2v of
                  SkipGram -> zip (repeat v) vectorized
                  CBOW     -> zip vectorized (repeat v)
                  _        -> error "unsupported word2vec method"

    cleanText :: String -> String
    cleanText string =
      let puncs = filter (`notElem` ['!', '"', '#', '$', '%', '(', ')', '.', '?', '\'']) (trim string)
          spacify = foldl (\acc x -> replace x ' ' acc) puncs [',', '/', '-', '\n', '\r']
          nonumber = filter (not . isNumber) spacify
          lower = map toLower nonumber
      in (unwords . words) lower -- remove unnecessary spaces
      where
        trim = f . f
          where
            f = reverse . dropWhile isSpace
        replace needle replacement =
          map (\c -> if c == needle then replacement else c)
