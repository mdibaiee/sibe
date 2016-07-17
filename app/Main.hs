module Main where

import Lib
import Numeric.LinearAlgebra
import Data.List
import Debug.Trace

-- 1x2
-- 2x3 + 1x3
-- 3x1 + 1x1

main :: IO ()
main =
  let learning_rate = 0.01
      ih = randomLayer 0 (2, 10)
      ho = randomLayer 1 (10, 1)
      network = ih :- O ho
      inputs = [vector [0, 1], vector [1, 1], vector [1, 0], vector [0, 0]]

      -- result = forward input network

      labels = [vector [1], vector [0], vector [1], vector [0]]

      updated_network = session inputs network labels learning_rate 100
      -- updated_network = train (head inputs) network (head labels) 0.5
      results = map (`forward` updated_network) inputs
  in print results
