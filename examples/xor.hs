module Main where
  import Sibe
  import Numeric.LinearAlgebra
  import Data.List
  import Debug.Trace

  main = do
    let learning_rate = 0.5
        (iterations, epochs) = (2, 1000)
        a = (sigmoid, sigmoid')
        rnetwork = randomNetwork 0 2 [(8, a)] (1, a) -- two inputs, 8 nodes in a single hidden layer, 1 output

        inputs = [vector [0, 1], vector [1, 0], vector [1, 1], vector [0, 0]]
        labels = [vector [1], vector [1], vector [0], vector [0]]

        initial_cost = zipWith crossEntropy (map (`forward` rnetwork) inputs) labels

        network = session inputs rnetwork labels learning_rate (iterations, epochs)
        results = map (`forward` network) inputs
        rounded = map (map round . toList) results

        cost = zipWith crossEntropy (map (`forward` network) inputs) labels

    putStrLn "parameters: "
    putStrLn $ "- inputs: " ++ show inputs
    putStrLn $ "- labels: " ++ show labels
    putStrLn $ "- learning rate: " ++ show learning_rate
    putStrLn $ "- iterations/epochs: " ++ show (iterations, epochs)
    putStrLn $ "- initial cost (cross-entropy): " ++ show initial_cost
    putStrLn "results: "
    putStrLn $ "- actual result: " ++ show results
    putStrLn $ "- rounded result: " ++ show rounded
    putStrLn $ "- cost (cross-entropy): " ++ show cost
