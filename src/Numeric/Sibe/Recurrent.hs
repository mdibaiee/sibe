{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Numeric.Sibe.Recurrent
  ( Recurrent (..)
  , randomRecurrent
  , processData
  , forward
  , predict
  , loss
  , backprop
  , sgd
  ) where
    import Numeric.LinearAlgebra
    import System.Random
    import System.Random.Shuffle
    import Debug.Trace
    import qualified Data.List as L
    import Data.Maybe
    import System.IO
    import Control.DeepSeq
    import Control.Monad
    import qualified Data.Vector.Storable as V
    import Data.Default.Class

    import qualified Graphics.Rendering.Chart.Easy as Chart
    import Graphics.Rendering.Chart.Backend.Cairo
    import Numeric.Sibe.Utils
    import Debug.Trace

    processData :: [String] -> ([(Int, String)], [[Int]])
    processData x =
      let setokens = map (\a -> " <start> " ++ a ++ " <end> ") x
          tokenized = map tokenize setokens
          vocabulary = zip [0..] (unique . concat $ tokenized)
          indexes = map (\a -> fst . fromJust $ L.find ((==a) . snd) vocabulary)
      in (vocabulary, map indexes tokenized)

    data Recurrent = Recurrent { bpttThreshold :: Int
                               , wordD :: Int
                               , hiddenD :: Int
                               , u :: Matrix Double
                               , v :: Matrix Double
                               , w :: Matrix Double
                               } deriving (Show, Read)
    instance Default Recurrent where
      def = Recurrent { bpttThreshold = 3
                      , hiddenD = 100
                      }

    randomRecurrent :: Seed -> Recurrent -> Recurrent
    randomRecurrent seed r = r { u = randomMatrix (wordD r, hiddenD r) (bounds $ wordD r)
                               , v = randomMatrix (hiddenD r, wordD r) (bounds $ hiddenD r)
                               , w = randomMatrix (hiddenD r, hiddenD r) (bounds $ hiddenD r)
                               }
      where
        randomMatrix (wr, wc) (l, u) = uniformSample (seed + wr + wc) wr $ replicate wc (l, u)
        bounds x = (negate . sqrt $ 1 / fromIntegral x, sqrt $ 1 / fromIntegral x)


    forward :: Recurrent -> [Int] -> (Matrix Double, Matrix Double)
    forward r input = 
      let (h, o) = helper [vector (replicate (hiddenD r) 0)] [] input
      in (fromRows h, fromRows o)
      where
        helper hs os [] = (hs, os)
        helper (h:hs) os (i:is) =
          let k = w r #> h
              newh = V.map tanh $ (u r ! i) + k
              o = softmax $ newh <# v r
          in helper (newh:h:hs) (o:os) is

    predict :: Recurrent -> [Int] -> [Int]
    predict r i = 
      let (_, o) = forward r i
      in map argmax (toLists o)

    backprop :: Recurrent -> [Int] -> Vector Double -> (Matrix Double, Matrix Double, Matrix Double)
    backprop r input y =
      let dU = zero (u r)
          dV = zero (v r)
          dW = zero (w r)
      in bp dU dV dW (zip [0..] input)
      where
        (hs, os) = forward r input
        -- delta
        dO = fromColumns $ zipWith (\i o -> if i `V.elem` y then o - 1 else o) [0..] (toColumns os)

        bp dU dV dW [] = (dU, dV, dW)
        bp dU dV dW ((i,x):xs) =
          let ndV = dV + (hs ! i) `outer` (dO ! i)
              dT = (v r) #> (dO ! i) -- * (1 - (hs ! i)^2)
              threshold = bpttThreshold r
              (ndU, ndW) = tt dU dW dT [max 0 (i-threshold)..i]
           in bp ndU ndV ndW xs
           where
             tt dU dW dT [] = (dU, dW)
             tt dU dW dT (c:cs) =
               let ndW = dW + (dT `outer` (hs ! (max 0 $ c - 1)))
                   zdT = vector $ replicate (V.length dT) 0
                   mdT = fromRows $ replicate (max 0 $ c - 1) zdT ++ [dT] ++ replicate (min (rows dU - 1) $ rows dU - c) zdT
                   ndU = dU + mdT
                   ndT = (w r) #> dT
               in tt ndU ndW ndT cs

        zero m = ((rows m)><(cols m)) $ repeat 0

    {-gradientCheck :: Recurrent -> [Int] -> Vector Double -> Double-}

    sgdStep :: Recurrent -> [Int] -> Vector Double -> Double -> Recurrent
    sgdStep r input y learningRate =
      let (dU, dV, dW) = backprop r input y
      in r { u = (u r) - scale learningRate dU
           , v = (v r) - scale learningRate dV
           , w = (w r) - scale learningRate dW
           }

    sgd :: Recurrent -> [[Int]] -> [Vector Double] -> Double -> Int -> Recurrent
    sgd r input y learningRate epochs = run [0..epochs] r
      where
        run [] r = r
        run (i:is) r = run is $ train (zip input y) r

        train [] r = r
        train ((x, y):xs) r = train xs $ sgdStep r x y learningRate

    softmax :: Vector Double -> Vector Double
    softmax x = cmap (\a -> exp a / s) x
      where
        s = V.sum $ exp x

    softmax' :: Vector Double -> Vector Double
    softmax' = cmap (\a -> sig a * (1 - sig a))
      where
        sig x = 1 / max (1 + exp (-x)) 1e-10

    -- cross-entropy
    loss :: Vector Double -> Vector Double -> Double
    loss ys os = (-1 / fromIntegral (V.length os)) * V.sum (V.zipWith f os ys)
      where
        f a y = y * log (max 1e-10 a)

