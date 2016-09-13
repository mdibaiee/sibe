module Sibe.Word2Vec
  (word2vec,
   Word2Vec (..)
  ) where
    import Sibe
    import Sibe.NLP
    import Sibe.Utils
    import Debug.Trace
    import Data.Char
    import Data.Maybe
    import Data.List
    import Numeric.LinearAlgebra hiding (find)
    import qualified Data.Vector.Storable as V
    import Data.Default.Class
    import Data.Function (on)

    data Word2Vec = Word2Vec { docs :: [String]
                             , window :: Int
                             }
    instance Default Word2Vec where
      def = Word2Vec { docs = []
                     , window = 2
                     }

    word2vec w2v session = do
      return trainingData
      let s = session { training = trainingData
                      , network = buildNetwork 0 (-1, 1) v [(v, 25, (id, one))] (20, v, (softmax, crossEntropy'))
                      , biases = False
                      }
      print trainingData
      newses <- run (gd . learningRateDecay (1.1, 0.1)) s

      let (hidden@(Layer biases nodes _) :- _) = network newses
      {-let computedVocVec = map (\(w, v) -> (w, forward v newses)) vocvec-}
      print biases
      let computedVocVec = map (\(w, v) -> (w, v <# nodes)) vocvec
      {-print computedVocVec-}

      {-mapM_ (\(w, v) -> do
                      putStr $ w ++ ": "
                      let similarities = map (similarity v . snd) computedVocVec
                      let sorted = sortBy (compare `on` similarity v . snd) computedVocVec
                      {-print $ zip (map fst sorted) similarities-}
                      print . take 2 . drop 1 . reverse $ map fst sorted
            ) computedVocVec-}

      return newses
      where
        ws = words (concatMap ((++ " <start> ") . map toLower) (docs w2v))
        vocabulary = ordNub ws
        v = length vocabulary

        cooccurence = foldl' iter [] (zip [0..] ws)
          where
            iter acc (i, w) =
              let a = findIndex ((== w) . fst) acc
                  before = take (window w2v) . drop (i - window w2v) $ ws
                  after = take (window w2v) . drop (i + 1) $ ws
                  ns = if i == 0 then after else before ++ after
              in
                if isJust a then
                  let idx = fromJust a
                      new = foldl (\acc n -> add acc n) (snd $ acc !! idx) ns
                  in take idx acc ++ [(w, new)] ++ drop (idx + 1) acc
                else
                  acc ++ [(w, map (\n -> (n, 1)) ns)]

            add [] n = [(n, 1)]
            add ((hw, hc):hs) n
              | n == hw = (hw, hc + 1):hs
              | otherwise = (hw, hc):add hs n

        vocvec = zip vocabulary $ map (onehot v) [0..v - 1]
        {-trainingData = map iter cooccurence
          where
            iter (w, targets) =
              let ts = map (\(w, c) -> c * (snd . fromJust $ find ((== w) . fst) vocvec)) targets
                  folded = foldl (+) (vector $ replicate v 0) ts
                  input = snd . fromJust $ find ((== w) . fst) vocvec
              in (input, folded)-}
        trainingData = map iter $ zip [window w2v..length vocvec - window w2v] vocvec
          where
            iter (i, (w, v)) =
              let before = take (window w2v) . drop (i - window w2v) $ vocvec
                  after = take (window w2v) . drop (i + 1) $ vocvec
                  ns = map snd $ before ++ after
                  new = foldl1 (+) ns
              in (v, new)

            add [] n = [(n, 1)]
            add ((hw, hc):hs) n
              | n == hw = (hw, hc + 1):hs
              | otherwise = (hw, hc):add hs n

        wordfrequency = foldl' iter [] ws
          where
            iter acc w =
              let i = findIndex ((== w) . fst) acc
              in
                if isJust i then
                  let idx = fromJust i
                  in take idx acc ++ [(w, snd (acc !! idx) + 1)] ++ drop (idx + 1) acc
                else
                  acc ++ [(w, 1)]
