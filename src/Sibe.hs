{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Sibe
    (Network(..),
     Layer,
     Input,
     Output,
     forward,
     randomLayer,
     randomNetwork,
     train,
     session,
     shuffle,
    ) where
      import Numeric.LinearAlgebra
      import System.Random
      import Debug.Trace
      import Data.List (foldl', sortBy)

      type LearningRate = Double
      type Input = Vector Double
      type Output = Vector Double

      data Layer = L { biases :: !(Vector Double)
                     , nodes  :: !(Matrix Double)
                     } deriving (Show)

      data Network = O Layer
                   | Layer :- Network
                   deriving (Show)
      infixr 5 :-

      runLayer :: Input -> Layer -> Output
      runLayer input (L !biases !weights) = input <# weights + biases

      forward :: Input -> Network -> Output
      forward input (O l) = logistic $ runLayer input l
      forward input (l :- n) = forward (logistic $ runLayer input l) n

      randomLayer :: Seed -> (Int, Int) -> Layer
      randomLayer seed (wr, wc) =
        let weights = uniformSample seed wr $ replicate wc (-1, 1)
            biases  = randomVector seed Uniform wc * 2 - 1
        in L biases weights

      randomNetwork :: Seed -> Int -> [Int] -> Int -> Network
      randomNetwork seed input [] output =
        O $ randomLayer seed (input, output)
      randomNetwork seed input (h:hs) output =
        randomLayer seed (input, h) :-
        randomNetwork (seed + 1) h hs output

      logistic :: Vector Double -> Vector Double
      logistic x = 1 / (1 + exp (-x))

      logistic' :: Vector Double -> Vector Double
      logistic' x = logistic x * (1 - logistic x)

      train :: Input
            -> Network
            -> Output -- target
            -> Double -- learning rate
            -> Network -- network's output
      train input network target alpha = fst $ run input network
        where
          run :: Input -> Network -> (Network, Vector Double)
          run input (O l@(L biases weights)) =
            let y = runLayer input l
                o = logistic y
                delta = o - target
                de = delta * logistic' y

                biases'  = biases  - scale alpha de
                weights' = weights - scale alpha (input `outer` de) -- small inputs learn slowly
                layer    = L biases' weights' -- updated layer

                pass = weights #> de
                -- pass = weights #> de

            in (O layer, pass)
          run input (l@(L biases weights) :- n) =
            let y = runLayer input l
                o = logistic y
                (n', delta) = run o n

                de = delta * logistic' y

                biases'  = biases  - scale alpha de
                weights' = weights - scale alpha (input `outer` de)
                layer = L biases' weights'

                pass = weights #> de
                -- pass = weights #> de
            in (layer :- n', pass)

      session :: [Input] -> Network -> [Output] -> Double -> (Int, Int) -> Network
      session inputs network labels alpha (iterations, epochs) =
        let n = length inputs
            indexes = shuffle n (map (`mod` n) [0..n * epochs])
        in foldl' iter network indexes
        where
          iter net i =
            let n = length inputs
                index = i `mod` n
                input = inputs !! index
                label = labels !! index
            in foldl' (\net _ -> train input net label alpha) net [0..iterations]

      shuffle :: Seed -> [a] -> [a]
      shuffle seed list =
        let ords = map ord $ take (length list) (randomRs (0, 1) (mkStdGen seed) :: [Int])
        in map snd $ sortBy (\x y -> fst x) (zip ords list)
        where ord x | x == 0 = LT
                    | x == 1 = GT
