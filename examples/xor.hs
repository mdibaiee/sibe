module Main where
  import Numeric.Sibe
  import Numeric.LinearAlgebra
  import Data.List
  import Debug.Trace
  import Data.Default.Class

  main = do
    let a = (sigmoid, sigmoid')
        rnetwork = randomNetwork 0 (-1, 1) 2 [(2, a)] (1, a) -- two inputs, 8 nodes in a single hidden layer, 1 output

        inputs = [vector [0, 1], vector [1, 0], vector [1, 1], vector [0, 0]]
        labels = [vector [1], vector [1], vector [0], vector [0]]

        session = def { network = rnetwork
                      , learningRate = 0.8
                      , epochs = 1000
                      , training = zip inputs labels
                      , test = zip inputs labels
                      } :: Session

        initialCost = crossEntropy session

    newsession <- run gd session

    let results = map (`forward` newsession) inputs
        rounded = map (map round . toList) results

        cost = crossEntropy newsession

    putStrLn "parameters: "
    putStrLn $ "- inputs: " ++ show inputs
    putStrLn $ "- labels: " ++ show labels
    putStrLn $ "- learning rate: " ++ show (learningRate session)
    putStrLn $ "- epochs: " ++ show (epochs session)
    putStrLn $ "- initial cost (cross-entropy): " ++ show initialCost
    putStrLn "results: "
    putStrLn $ "- actual result: " ++ show results
    putStrLn $ "- rounded result: " ++ show rounded
    putStrLn $ "- cost (cross-entropy): " ++ show cost
