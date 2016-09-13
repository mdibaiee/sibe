{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Sibe
    (Network(..),
     Layer(..),
     Input,
     Output,
     Activation,
     forward,
     runLayer,
     randomLayer,
     randomNetwork,
     buildNetwork,
     saveNetwork,
     loadNetwork,
     train,
     gd,
     sgd,
     run,
     sigmoid,
     sigmoid',
     softmax,
     softmax',
     relu,
     relu',
     crossEntropy,
     crossEntropy',
     genSeed,
     replaceVector,
     Session(..),
     accuracy,
     learningRateDecay,
     ignoreBiases,
     one
    ) where
      import Numeric.LinearAlgebra
      import System.Random
      import System.Random.Shuffle
      import Debug.Trace
      import Data.List (foldl', sortBy, genericLength, permutations)
      import System.IO
      import Control.DeepSeq
      import Control.Monad
      import qualified Data.Vector.Storable as V
      import Data.Default.Class
      import System.Exit

      import qualified Graphics.Rendering.Chart.Easy as Chart
      import Graphics.Rendering.Chart.Backend.Cairo

      type LearningRate = Double
      type Input = Vector Double
      type Output = Vector Double
      type Activation = (Vector Double -> Vector Double, Vector Double -> Vector Double)

      data Layer = Layer { biases     :: !(Vector Double)
                         , nodes      :: !(Matrix Double)
                         , activation :: Activation
                         }

      instance Show Layer where
        show (Layer biases nodes _) = "(" ++ show biases ++ "," ++ show nodes ++ ")"

      data Network = O Layer
                   | Layer :- Network
      
      instance Show Network where
        show (Layer biases nodes _ :- n) =
          (show . length $ toLists nodes) ++ "x" ++ (show . length . head . toLists $ nodes) ++ " " ++ (show . length . toList $ biases) ++ " :- " ++ show n
        show (O (Layer biases nodes _)) =
          (show . length $ toLists nodes) ++ "x" ++ (show . length . head . toLists $ nodes) ++ " " ++ (show . length . toList $ biases)
                   
      infixr 5 :-

      data Session = Session  { network      :: Network
                              , training     :: [(Vector Double, Vector Double)]
                              , test         :: [(Vector Double, Vector Double)]
                              , learningRate :: Double
                              , epochs       :: Int
                              , epoch        :: Int
                              , batchSize    :: Int
                              , chart        :: [(Int, Double, Double)]
                              , momentum     :: Double
                              } deriving (Show)

      emptyNetwork = randomNetwork 0 (0, 0) 0 [] (0, (id, id))
      instance Default Session where
        def = Session { network      = seq (die "You have not specified a network parameter") emptyNetwork
                      , training     = seq (die "You have not specified training data") []
                      , test         = seq (die "You have not specified test data") []
                      , learningRate = 0.5
                      , epochs       = 35
                      , epoch        = 0
                      , batchSize    = 0
                      , chart        = []
                      , momentum     = 0
                      }

      saveNetwork :: Network -> String -> IO ()
      saveNetwork network file =
        writeFile file ((show . reverse) (gen network []))
        where
          gen (O (Layer biases nodes _)) list = (biases, nodes) : list
          gen (Layer biases nodes _ :- n) list = gen n $ (biases, nodes) : list

      loadNetwork :: [Activation] -> String -> IO Network
      loadNetwork activations file = do
        handle <- openFile file ReadMode
        content <- hGetContents handle
        let list = read content :: [(Vector Double, Matrix Double)]
            network = gen list activations
        content `deepseq` hClose handle
        return network

        where
          gen [(biases, nodes)] [a] = O (Layer biases nodes a)
          gen ((biases, nodes):hs) (a:as) = Layer biases nodes a :- gen hs as

      runLayer :: Input -> Layer -> Output
      runLayer input (Layer !biases !weights _) = input <# weights + biases

      forward :: Input -> Session -> Output
      forward input session = compute input (network session)
        where
          compute input (O l@(Layer _ _ (fn, _))) = fn $ runLayer input l
          compute input (l@(Layer _ _ (fn, _)) :- n) = compute ((fst . activation $ l) $ runLayer input l) n

      randomLayer :: Seed -> (Int, Int) -> (Double, Double) -> Activation -> Layer
      randomLayer seed (wr, wc) (l, u) =
        let weights = uniformSample seed wr $ replicate wc (l, u)
            biases  = randomVector seed Uniform wc * realToFrac u - realToFrac l
        in Layer biases weights

      randomNetwork :: Seed -> (Double, Double) -> Int -> [(Int, Activation)] -> (Int, Activation) -> Network
      randomNetwork seed bound input [] (output, a) =
        O $ randomLayer seed (input, output) bound a
      randomNetwork seed bound input ((h, a):hs) output =
        randomLayer seed (input, h) bound a :-
        randomNetwork (seed + 1) bound h hs output

      buildNetwork :: Seed -> (Double, Double) -> Int -> [(Int, Int, Activation)] -> (Int, Int, Activation) -> Network
      buildNetwork seed bound input [] (outputRows, outputColumns, a) =
        O $ randomLayer seed (input, outputColumns) bound a
      buildNetwork seed bound input ((rows, columns, a):hs) output =
        randomLayer seed (input, columns) bound a :-
        buildNetwork (seed + 1) bound columns hs output

      sigmoid :: Vector Double -> Vector Double
      sigmoid x = 1 / max (1 + exp (-x)) 1e-10

      sigmoid' :: Vector Double -> Vector Double
      sigmoid' x = sigmoid x * (1 - sigmoid x)

      softmax :: Vector Double -> Vector Double
      softmax x = cmap (\a -> exp a / s) x
        where
          s = V.sum $ exp x

      softmax' :: Vector Double -> Vector Double
      softmax' = cmap (\a -> sig a * (1 - sig a))
        where
          sig x = 1 / max (1 + exp (-x)) 1e-10

      relu :: Vector Double -> Vector Double
      relu = cmap (max 0.1)

      relu' :: Vector Double -> Vector Double
      relu' = cmap dev
        where dev x
                | x < 0 = 0
                | otherwise = 1

      crossEntropy :: Session -> Double
      crossEntropy session =
        let inputs = map fst (test session)
            labels = map (toList . snd) (test session)
            outputs = map (toList . (`forward` session)) inputs
            pairs = zip outputs labels
            n = genericLength pairs
        in sum (map set pairs) / n
        where
          set (os, ls) = (-1 / genericLength os) * sum (zipWith f os ls)
          f a y = y * log (max 1e-10 a)

      crossEntropy' :: Vector Double -> Vector Double
      crossEntropy' x = 1 / fromIntegral (V.length x)

      one :: Vector Double -> Vector Double
      one v = vector $ replicate (V.length v) 1

      train :: Input
            -> Network
            -> Output -- target
            -> Double -- learning rate
            -> Network -- network's output
      train input network target alpha = fst $ run input network
        where
          run :: Input -> Network -> (Network, Vector Double)
          run input (O l@(Layer biases weights (fn, fn'))) =
            let y = runLayer input l
                o = fn y
                delta = o - target 
                de = delta * fn' y
                -- de = delta / fromIntegral (V.length o) -- cross entropy cost

                biases'  = biases  - scale alpha de
                weights' = weights - scale alpha (input `outer` de) -- small inputs learn slowly
                layer    = Layer biases' weights' (fn, fn') -- updated layer

                pass = weights #> de
                -- pass = weights #> de

            in (O layer, pass)
          run input (l@(Layer biases weights (fn, fn')) :- n) =
            let y = runLayer input l
                o = fn y
                (n', delta) = run o n

                de = delta * fn' y

                biases'  = biases  - cmap (*alpha) de
                weights' = weights - cmap (*alpha) (input `outer` de)
                layer = Layer biases' weights' (fn, fn')

                pass = weights #> de
                -- pass = weights #> de
            in (layer :- n', pass)

      gd :: Session -> IO Session
      gd session = do
        seed <- newStdGen

        let pairs = training session
            alpha = learningRate session
            net = network session

        let n = length pairs

        shuffled <- shuffleM pairs

        let newnet = foldl' (\n (input, label) -> train input n label alpha) net pairs

        return session { network = newnet
                       , epoch = epoch session + 1
                       }

      sgd :: Session -> IO Session
      sgd session = do
        seed <- newStdGen

        let pairs = training session
            bsize = batchSize session
            alpha = learningRate session
            net = network session

        let n = length pairs
            iterations = n `div` bsize - 1

        shuffled <- shuffleM pairs

        let iter net i =
              let n = length pairs
                  batch = take bsize . drop (i * bsize) $ shuffled
                  batchInputs = map fst batch
                  batchLabels = map snd batch
                  batchPair = zip batchInputs batchLabels
              in foldl' (\n (input, label) -> train input n label alpha) net batchPair

        let newnet = foldl' iter net [0..iterations]
            cost = crossEntropy (session { network = newnet })

        let el = map (\(e, l, _) -> (e, l)) (chart session)
            ea = map (\(e, _, a) -> (e, a)) (chart session)

        putStrLn $ (show $ epoch session) ++ " => " ++ (show cost) ++ " @ " ++ (show $ learningRate session)

        toFile Chart.def "sgd.png" $ do
          Chart.layoutlr_title Chart..= "loss over time"
          Chart.plotLeft (Chart.line "loss" [el])
          Chart.plotRight (Chart.line "learningRate" [ea])

        return session { network = newnet
                       , epoch = epoch session + 1
                       , chart = (epoch session, cost, learningRate session):chart session
                       }


      accuracy :: Session -> Double
      accuracy session = 
        let inputs = map fst (test session)
            labels = map snd (test session)

            results = map (`forward` session) inputs
            rounded = map (map round . toList) results

            equals = zipWith (==) rounded (map (map round . toList) labels)
        in genericLength (filter (== True) equals) / genericLength inputs

      learningRateDecay :: (Double, Double) -> Session -> Session
      learningRateDecay (step, m) session =
        session { learningRate = max m $ learningRate session / step }

      ignoreBiases :: Session -> Session
      ignoreBiases session =
        session { network = rmbias (network session) }
        where
          rmbias (O (Layer nodes biases a)) = O $ Layer nodes (biases * 0) a
          rmbias ((Layer nodes biases a) :- n) = Layer nodes (biases * 0) a :- rmbias n

      run :: (Session -> IO Session)
          ->  Session -> IO Session
      run fn session = foldM (\s i -> fn s) session [0..epochs session]

      factorial :: Int -> Int
      factorial 0 = 1
      factorial x = x * factorial (x - 1)

      genSeed :: IO Seed
      genSeed = do
        (seed, _) <- random <$> newStdGen :: IO (Int, StdGen)
        return seed

      replaceVector :: Vector Double -> Int -> Double -> Vector Double
      replaceVector vec index value =
        let list = toList vec
        in fromList $ take index list ++ value : drop (index + 1) list

      clip :: Double -> (Double, Double) -> Double
      clip x (l, u) = min u (max l x)
