module Sibe.Utils
  ( similarity
  , ordNub
  , onehot
  , average
  ) where
    import qualified Data.Vector.Storable as V
    import qualified Data.Set as Set
    import Numeric.LinearAlgebra

    similarity :: Vector Double -> Vector Double -> Double
    similarity a b = (V.sum $ a * b) / (magnitude a * magnitude b)
      where
        magnitude :: Vector Double -> Double
        magnitude v = sqrt $ V.sum (cmap (^2) v)

    onehot :: Int -> Int -> Vector Double
    onehot len i = vector $ replicate i 0 ++ [1] ++ replicate (len - i - 1) 0

    ordNub :: (Ord a) => [a] -> [a]
    ordNub = go Set.empty
      where
        go _ [] = []
        go s (x:xs) = if x `Set.member` s then go s xs
                                          else x : go (Set.insert x s) xs

    average :: Vector Double -> Vector Double
    average v = cmap (/ (V.sum v)) v
