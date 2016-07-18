module Main where

import Sibe
import Numeric.LinearAlgebra
import Data.List
import Debug.Trace

-- 1x2
-- 2x3 + 1x3
-- 3x1 + 1x1

-- main :: IO [()]
main =
  let learning_rate = 0.5
      ih = randomLayer 0 (2, 8)
      ho = randomLayer 1 (8, 1)
      network = ih :- O ho

      inputs = [vector [0, 1], vector [1, 0], vector [1, 1], vector [0, 0]]
      labels = [vector [1], vector [1], vector [0], vector [0]]

      updated_network = session inputs network labels learning_rate (2, 1000)
      results = map (`forward` updated_network) inputs
  in print results
