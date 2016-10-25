module Numeric.Sibe.Utils
  ( similarity
  , ordNub
  , onehot
  , average
  , pca
  , tokenize
  , frequency
  , unique
  , argmax
  , shape
  ) where
    import qualified Data.Vector.Storable as V
    import qualified Data.Set as Set
    import Numeric.LinearAlgebra
    import Data.List.Split
    import Data.Char (isSpace, isNumber, toLower)
    import Control.Arrow ((&&&))
    import Data.List

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
    unique :: (Ord a) => [a] -> [a]
    unique = ordNub

    average :: Vector Double -> Vector Double
    average v = cmap (/ (V.sum v)) v

    pca :: Matrix Double -> Int -> Matrix Double
    pca m d =
      let rs = toRows m
          means = map (\v -> V.sum v / fromIntegral (V.length v)) rs
          meanReduced = map (\(a, b) -> V.map (+ (negate b)) a) $ zip rs means
          mat = fromRows meanReduced
          
          (u, s, v) = svd mat
          diagS = diagRect 0 s (rows mat) (cols mat)

      in u ?? (All, Take d) <> diagS ?? (Take d, Take d)

    tokenize :: String -> [String]
    tokenize str = 
      let spaced = spacify str
          ws = words spaced
      in ws
      where
        puncs = ['!', '"', '#', '$', '%', '(', ')', '.', '?', ',', '\'', '/', '-']
        replace needle replacement =
          concatMap (\c -> if c == needle then replacement else c)
        spacify = foldl (\acc c -> if c `elem` puncs then acc ++ [' ', c, ' '] else acc ++ [c]) ""

    frequency :: (Ord a) => [a] -> [(a, Int)]
    frequency = map (head &&& length) . group . sort

    argmax :: (Foldable t, Num a, Fractional a, Ord a) => t a -> Int
    argmax v = snd $ foldl mx ((-1/0), 0) v
      where
        mx (a, i) b
          | b > a = (b, i + 1)
          | otherwise = (a, i)

    shape :: Matrix a -> (Int, Int)
    shape x = (rows x, cols x)
