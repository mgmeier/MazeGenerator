module  RandomGeneration where

import  Types

import  System.Random                   (randomRIO)


randomElement :: [a] -> IO a
randomElement []    = fail "randomElement: empty list"
randomElement [x]   = return x
randomElement xs    = (xs !!) `fmap` randomRIO (0, length xs - 1)


-- Implementations for GenerationBias.
-- The basic functionality is shifting the odds between randomly
-- choosing the horizontal or vertical neighbour of some given
-- maze cell; different odds result in the desired visual pattern.
randomBias :: GenerationBias -> (Int, Int) -> MazeIx -> [MazeIx] -> IO MazeIx
randomBias NoBias _ _ xs =
    randomElement xs

randomBias CheckerBoard dims@(mazeW, mazeH) ix@(x, y) xs =
    let
        sqX     = 2*x `div` mazeW
        sqY     = 2*y `div` mazeH
        bias    = bool HorizBias VertBias $
            (odd sqY && even sqX) || (odd sqX && even sqY)
    in randomBias bias dims ix xs

randomBias DiagonalSplit dims@(mazeW, mazeH) ix@(x, y) xs =
    let bias    = bool VertBias HorizBias $ x > (y * mazeW) `div` mazeH
    in randomBias bias dims ix xs    

randomBias bias _ (x, y) xs =
    let biased = filter filterFunc xs
    in randomElement $ concat $ xs : replicate 3 biased

  where 
    filterFunc = case bias of
        VertBias    -> (== x) . fst
        _           -> (== y) . snd                                     -- other biases than HorizBias matched beforehand
