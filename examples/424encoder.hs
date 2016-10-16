module Main where
  import Numeric.Sibe
  import Numeric.LinearAlgebra
  import Data.List
  import Debug.Trace
  import Data.Default.Class

  main = do
    let alpha = 0.5
        epochs = 1000
        a = (sigmoid, sigmoid')
        rnetwork = randomNetwork 0 (-0.1, 0.1) 4 [(2, a)] (4, a)

        inputs = [vector [1, 0, 0, 0],
                  vector [0, 1, 0, 0],
                  vector [0, 0, 1, 0],
                  vector [0, 0, 0, 1]]

        labels = [vector [1, 0, 0, 0],
                  vector [0, 1, 0, 0],
                  vector [0, 0, 1, 0],
                  vector [0, 0, 0, 1]]

        session = def { network = rnetwork
                      , learningRate = 0.5
                      , epochs = 1000
                      , training = zip inputs labels
                      , test = zip inputs labels
                      } :: Session

    let initialCost = crossEntropy session

    newsession <- run gd session

    let results = map (`forward` newsession) inputs
        rounded = map (map round . toList) results

        cost = crossEntropy newsession

    putStrLn "parameters: "
    putStrLn $ "- inputs: " ++ show inputs
    putStrLn $ "- labels: " ++ show labels
    putStrLn $ "- learning rate: " ++ show alpha
    putStrLn $ "- epochs: " ++ show epochs
    putStrLn $ "- initial cost (cross-entropy): " ++ show initialCost
    putStrLn "results: "
    putStrLn $ "- actual result: " ++ show results
    putStrLn $ "- rounded result: " ++ show rounded
    putStrLn $ "- cost (cross-entropy): " ++ show cost
