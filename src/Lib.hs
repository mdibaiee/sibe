{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    (Network(..),
     Layer,
     Input,
     Output,
     forward,
     randomLayer,
     train,
     session,
    ) where
      import Numeric.LinearAlgebra
      import Control.Monad.Random
      import System.Random
      import Debug.Trace
      import Data.List (foldl')

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
      forward input (O l) = cmap logistic $ runLayer input l
      forward input (l :- n) = forward (cmap logistic $ runLayer input l) n

      randomLayer :: Seed -> (Int, Int) -> Layer
      randomLayer seed (wr, wc) =
        let weights = uniformSample seed wr $ replicate wc (-1, 1)
            biases  = randomVector seed Uniform wc * 2 - 1
        in L biases weights

      logistic :: Double -> Double
      logistic x = 1 / (1 + exp (-x))

      logistic' :: Double -> Double
      logistic' x = logistic x / max 1e-8 (1 - logistic x)

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
                o = cmap logistic y
                delta = o - target
                de = delta * cmap logistic' y

                biases'  = biases  - scale alpha de
                weights' = weights - scale alpha (input `outer` de) -- small inputs learn slowly
                layer    = L biases' weights' -- updated layer

                pass = weights #> de
                -- pass = weights #> de

            in (O layer, pass)
          run input (l@(L biases weights) :- n) =
            let y = runLayer input l
                o = cmap logistic y
                (n', delta) = run o n

                de = delta * cmap logistic' y

                biases'  = biases  - scale alpha de
                weights' = weights - scale alpha (input `outer` de)
                layer = L biases' weights'

                pass = weights #> de
                -- pass = weights #> de
            in (layer :- n', pass)

      session :: [Input] -> Network -> [Output] -> Double -> Int -> Network
      session inputs network labels alpha epochs =
        foldl' iter network [0..n * epochs]
        where
          iter net i =
            let n = length inputs - 1
                index = i `mod` n
                input = inputs !! index
                label = labels !! index
            in train input net label alpha
