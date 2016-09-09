module Main where
  import Sibe
  import Numeric.LinearAlgebra
  import Data.List
  import Debug.Trace

  main = do
    let alpha = 0.5
        epochs = 1000
        a = (sigmoid, sigmoid')
        lo = (sigmoid, (\_ -> 1)) -- cross entropy
        -- a = (relu, relu')
        rnetwork = randomNetwork 0 (-1, 1) 1 [(50, a)] (1, lo)

        inputs = map (\a -> vector [a]) (reverse [0, 30, 45, 60, 90])
        labels = map (\deg -> vector $ [sin $ deg * pi/180]) (reverse [0, 30, 45, 60, 90])

        initial_cost = zipWith crossEntropy (map (`forward` rnetwork) inputs) labels

    network <- run session inputs rnetwork labels alpha epochs

    let results = map (`forward` network) inputs
        rounded = map (map round . toList) results

        cost = zipWith crossEntropy (map (`forward` network) inputs) labels

    putStrLn "parameters: "
    putStrLn $ "- inputs: " ++ show inputs
    putStrLn $ "- labels: " ++ show labels
    putStrLn $ "- learning rate: " ++ show alpha
    putStrLn $ "- epochs: " ++ show epochs
    {-putStrLn $ "- initial cost (cross-entropy): " ++ show initial_cost-}
    putStrLn "results: "
    putStrLn $ "- actual result: " ++ show results
    {-putStrLn $ "- cost (cross-entropy): " ++ show cost-}
