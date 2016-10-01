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
    import Control.Monad
    import System.Random

    import Graphics.Rendering.Chart as Chart
    import Graphics.Rendering.Chart.Backend.Cairo
    import Control.Lens

    data W2VMethod = SkipGram | CBOW
    data Word2Vec = Word2Vec { docs :: [String]
                             , window :: Int
                             , dimensions :: Int
                             , method :: W2VMethod
                             , w2vChartName :: String
                             , w2vDrawChart :: Bool
                             }
    instance Default Word2Vec where
      def = Word2Vec { docs = []
                     , window = 2
                     , w2vChartName = "w2v.png"
                     , w2vDrawChart = False
                     }

    word2vec w2v session = do
      seed <- newStdGen

      let s = session { training = trainingData
                      , network = randomNetwork 0 (-1, 1) v [(dimensions w2v, (id, one))] (v, (softmax, crossEntropy'))
                      }

      when (debug s) $ do
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

      when (w2vDrawChart w2v) $ do
        let mat = fromColumns . map snd $ computedVocVec
            (u, s, v) = svd mat
            cut = subMatrix (0, 0) (2, cols mat)
            diagS = diagRect 0 (V.take 2 s) (rows mat) (cols mat)

            twoDimensions = cut $ u <> diagS <> tr v
            textData = zipWith (\s l -> (V.head l, V.last l, s)) (map fst computedVocVec) (toColumns twoDimensions)

            chart = toRenderable layout
              where
                textP  = plot_annotation_values .~ textData
                      $ def
                layout = layout_title .~ "word vectors"
                      $ layout_plots .~ [toPlot textP]
                      $ def
                    
        renderableToFile def (w2vChartName w2v) chart
        return ()

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
                if length wds <= 1
                  then []
                  else
                    case method w2v of
                      SkipGram -> [(v, average new)]
                      CBOW     -> [(average new, v)]
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
