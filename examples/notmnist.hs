{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
  import Sibe
  import Numeric.LinearAlgebra
  import Data.List
  import Debug.Trace
  import System.IO
  import System.Directory
  import Codec.Picture
  import Codec.Picture.Types
  import qualified Data.Vector.Storable as V
  import Data.Either
  import System.Random
  import System.Random.Shuffle
  import Data.Default.Class

  import qualified Graphics.Rendering.Chart.Easy as Chart
  import Graphics.Rendering.Chart.Backend.Cairo

  main = do
    -- random seed, you might comment this line to get real random results
    setStdGen (mkStdGen 100)

    let a         = (sigmoid, sigmoid')
        o         = (softmax, crossEntropy')
        rnetwork  = randomNetwork 0 (-1, 1) (28*28) [(100, a)] (10, o)

    (inputs, labels) <- dataset

    let trp      = length inputs * 70 `div` 100
        tep      = length inputs * 30 `div` 100

        -- training data
        trinputs = take trp inputs
        trlabels = take trp labels

        -- test data
        teinputs = take tep . drop trp $ inputs
        telabels = take tep . drop trp $ labels

    let session = def { learningRate = 0.5
                      , batchSize    = 32
                      , epochs       = 10
                      , network      = rnetwork
                      , training     = zip trinputs trlabels
                      , test         = zip teinputs telabels
                      } :: Session

    let initialCost = crossEntropy session

    newsession <- run (sgd . learningRateDecay (1.1, 5e-2)) session

    let el = map (\(e, l, _) -> (e, l)) (chart newsession)
        ea = map (\(e, _, a) -> (e, a)) (chart newsession)
    toFile Chart.def "notmnist.png" $ do
      Chart.layoutlr_title Chart..= "loss over time"
      Chart.plotLeft (Chart.line "loss" [el])
      Chart.plotRight (Chart.line "learningRate" [ea])


    let cost = crossEntropy newsession

    putStrLn "parameters: "
    putStrLn $ "- batch size: " ++ show (batchSize session)
    putStrLn $ "- learning rate: " ++ show (learningRate session)
    putStrLn $ "- epochs: " ++ show (epochs session)
    putStrLn $ "- initial cost (cross-entropy): " ++ show initialCost
    putStrLn "results: "
    putStrLn $ "- accuracy: " ++ show (accuracy newsession)
    putStrLn $ "- cost (cross-entropy): " ++ show cost

  dataset :: IO ([Vector Double], [Vector Double])
  dataset = do
    let dir = "examples/notMNIST/"
    
    groups <- filter ((/= '.') . head) <$> listDirectory dir

    inputFiles <- mapM (listDirectory . (dir ++)) groups

    let n = 512 {-- minimum (map length inputFiles) --}
        numbers = map (`div` n) [0..n * length groups - 1]
        inputFilesFull = map (\(i, g) -> map ((dir ++ i ++ "/") ++) g) (zip groups inputFiles)


    inputImages <- mapM (mapM readImage . take n) inputFilesFull

    let names = map (take n) inputFilesFull

    let (l, r) = partitionEithers $ concat inputImages
        inputs = map (fromPixels . convertRGB8) r
        labels = map (\i -> V.replicate i 0 `V.snoc` 1 V.++ V.replicate (9 - i) 0) numbers

        pairs  = zip inputs labels

    shuffled <- shuffleM pairs
    return (map fst shuffled, map snd shuffled)

    where
      fromPixels :: Image PixelRGB8 -> Vector Double
      fromPixels img@Image { .. } =
        let pairs = [(x, y) | x <- [0..imageWidth - 1], y <- [0..imageHeight - 1]]
        in V.fromList $ map iter pairs
        where
          iter (x, y) =
            let (PixelRGB8 r g b) = convertPixel $ pixelAt img x y
            in
              if r == 0 && g == 0 && b == 0 then 0 else 1

    
