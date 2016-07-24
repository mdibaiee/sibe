{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Sibe
    (Network(..),
     Layer,
     Input,
     Output,
     Activation,
     forward,
     randomLayer,
     randomNetwork,
     saveNetwork,
     loadNetwork,
     train,
     session,
     shuffle,
     logistic,
     logistic',
     crossEntropy,
     genSeed,
     replaceVector
    ) where
      import Numeric.LinearAlgebra
      import System.Random
      import Debug.Trace
      import Data.List (foldl', sortBy)
      import System.IO
      import Control.DeepSeq

      type LearningRate = Double
      type Input = Vector Double
      type Output = Vector Double
      type Activation = (Vector Double -> Vector Double, Vector Double -> Vector Double)

      data Layer = L { biases     :: !(Vector Double)
                     , nodes      :: !(Matrix Double)
                     , activation :: Activation
                     }

      instance Show Layer where
        show (L biases nodes _) = "(" ++ show biases ++ "," ++ show nodes ++ ")"

      data Network = O Layer
                   | Layer :- Network
                   deriving (Show)
      infixr 5 :-

      saveNetwork :: Network -> String -> IO ()
      saveNetwork network file =
        writeFile file ((show . reverse) (gen network []))
        where
          gen (O (L biases nodes _)) list = (biases, nodes) : list
          gen (L biases nodes _ :- n) list = gen n $ (biases, nodes) : list

      loadNetwork :: [Activation] -> String -> IO Network
      loadNetwork activations file = do
        handle <- openFile file ReadMode
        content <- hGetContents handle
        let list = read content :: [(Vector Double, Matrix Double)]
            network = gen list activations
        content `deepseq` hClose handle
        return network

        where
          gen [(biases, nodes)] [a] = O (L biases nodes a)
          gen ((biases, nodes):hs) (a:as) = L biases nodes a :- gen hs as

      runLayer :: Input -> Layer -> Output
      runLayer input (L !biases !weights _) = input <# weights + biases

      forward :: Input -> Network -> Output
      forward input (O l@(L _ _ (fn, _))) = fn $ runLayer input l
      forward input (l@(L _ _ (fn, _)) :- n) = forward ((fst . activation $ l) $ runLayer input l) n

      randomLayer :: Seed -> (Int, Int) -> Activation -> Layer
      randomLayer seed (wr, wc) =
        let weights = uniformSample seed wr $ replicate wc (-1, 1)
            biases  = randomVector seed Uniform wc * 2 - 1
        in L biases weights

      randomNetwork :: Seed -> Int -> [(Int, Activation)] -> (Int, Activation) -> Network
      randomNetwork seed input [] (output, a) =
        O $ randomLayer seed (input, output) a
      randomNetwork seed input ((h, a):hs) output =
        randomLayer seed (input, h) a :-
        randomNetwork (seed + 1) h hs output

      logistic :: Vector Double -> Vector Double
      logistic x = 1 / (1 + exp (-x))

      logistic' :: Vector Double -> Vector Double
      logistic' x = logistic x * (1 - logistic x)

      crossEntropy :: Output -> Output -> Double
      crossEntropy output target =
        let pairs = zip (toList output) (toList target)
            n = fromIntegral (length pairs)
        in (-1 / n) * sum (map f pairs)
        where
          f (a, y) = y * log a + (1 - y) * log (1 - a)

      train :: Input
            -> Network
            -> Output -- target
            -> Double -- learning rate
            -> Network -- network's output
      train input network target alpha = fst $ run input network
        where
          run :: Input -> Network -> (Network, Vector Double)
          run input (O l@(L biases weights (fn, fn'))) =
            let y = runLayer input l
                o = fn y
                delta = o - target
                -- de = delta * fn' y -- quadratic cost
                de = delta -- cross entropy cost

                biases'  = biases  - scale alpha de
                weights' = weights - scale alpha (input `outer` de) -- small inputs learn slowly
                layer    = L biases' weights' (fn, fn') -- updated layer

                pass = weights #> de
                -- pass = weights #> de

            in (O layer, pass)
          run input (l@(L biases weights (fn, fn')) :- n) =
            let y = runLayer input l
                o = fn y
                (n', delta) = run o n

                de = delta * fn' y -- quadratic cost

                biases'  = biases  - scale alpha de
                weights' = weights - scale alpha (input `outer` de)
                layer = L biases' weights' (fn, fn')

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

      genSeed :: IO Seed
      genSeed = do
        (seed, _) <- random <$> newStdGen :: IO (Int, StdGen)
        return seed

      replaceVector :: Vector Double -> Int -> Double -> Vector Double
      replaceVector vec index value =
        let list = toList vec
        in fromList $ rrow index list
        where
          rrow index [] = []
          rrow index (x:xs)
            | index == index = value:xs
            | otherwise = x : rrow (index + 1) xs
