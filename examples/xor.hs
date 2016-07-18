module Main where
  import Sibe
  import Numeric.LinearAlgebra
  import Data.List
  import Debug.Trace

  main = do
    let learning_rate = 0.5
        (iterations, epochs) = (2, 1000)
        rnetwork = randomNetwork 0 2 [8] 1 -- two inputs, 8 nodes in a single hidden layer, 1 output

        inputs = [vector [0, 1], vector [1, 0], vector [1, 1], vector [0, 0]]
        labels = [vector [1], vector [1], vector [0], vector [0]]

        network = session inputs rnetwork labels learning_rate (iterations, epochs)
        results = map (`forward` network) inputs
        rounded = map (map round . toList) results

    putStrLn "parameters: "
    putStrLn $ "- inputs: " ++ show inputs
    putStrLn $ "- labels: " ++ show labels
    putStrLn $ "- learning rate: " ++ show learning_rate
    putStrLn $ "- iterations/epochs: " ++ show (iterations, epochs)
    putStrLn "results: "
    putStrLn $ "- actual result: " ++ show results
    putStrLn $ "- rounded result: " ++ show rounded
