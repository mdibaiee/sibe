sibe
====

A simple Machine Learning library.

A simple neural network:
```haskell
module Main where
  import Sibe
  import Numeric.LinearAlgebra
  import Data.List

  main = do
    let learning_rate = 0.5
        (iterations, epochs) = (2, 1000)
        a = (logistic, logistic') -- activation function and the derivative
        rnetwork = randomNetwork 0 2 [(8, a)] (1, a) -- two inputs, 8 nodes in a single hidden layer, 1 output

        inputs = [vector [0, 1], vector [1, 0], vector [1, 1], vector [0, 0]] -- training dataset
        labels = [vector [1], vector [1], vector [0], vector [0]] -- training labels

        -- initial cost using crossEntropy method
        initial_cost = zipWith crossEntropy (map (`forward` rnetwork) inputs) labels

        -- train the network
        network = session inputs rnetwork labels learning_rate (iterations, epochs)

        -- run inputs through the trained network
        -- note: here we are using the examples in the training dataset to test the network,
        --       this is here just to demonstrate the way the library works, you should not do this
        results = map (`forward` network) inputs

        -- compute the new cost
        cost = zipWith crossEntropy (map (`forward` network) inputs) labels
```

See other examples:
```
stack exec example-xor
stack exec example-naivebayes-doc-classifier
```
