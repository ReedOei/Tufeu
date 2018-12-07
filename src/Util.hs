module Util where

import Control.Monad
import Control.Monad.IO.Class

import System.Random

withChance :: MonadIO m => Integer -> a -> a -> m a
withChance chance ifYes ifNo = do
    i <- liftIO $ randomRIO (0, chance)

    if i == 0 then
        pure $ ifYes
    else
        pure $ ifNo

minMaxRange :: (Ord a, Random a, MonadIO m) => (a, a) -> (a, a) -> m (a, a)
minMaxRange minRange maxRange = do
    minVal <- randRange minRange
    maxVal <- max minVal <$> randRange maxRange

    pure (minVal, maxVal)

randRange :: (Random a, MonadIO m) => (a, a) -> m a
randRange = liftIO . randomRIO

weightedChoice :: MonadIO m => [(Integer, a)] -> m a
weightedChoice choices = do
    let s = sum $ map fst choices
    w <- randRange (0, s)
    pure $ weightedChoice' w choices
    where
        weightedChoice' _ [(_, v)] = v
        weightedChoice' w ((choiceWeight, v):rest)
            | w < choiceWeight = v
            | otherwise = weightedChoice' (w - choiceWeight) rest

choice :: MonadIO m => [a] -> m a
choice list = do
    i <- liftIO $ randomRIO (0, length list - 1)
    pure $ list !! i

randomList :: MonadIO m => [a] -> m [a]
randomList choices = do
    g <- liftIO newStdGen
    let idxs = randomRs (0, length choices - 1) g
    pure $ map (choices !!) idxs

takeRandom :: (Random a, Integral a, MonadIO m) => (a, a) -> [b] -> m [b]
takeRandom nRange l = do
    num <- randRange nRange
    repM num l

maybeChoice :: MonadIO m => Integer -> [a] -> m (Maybe a)
maybeChoice chance l = do
    c <- choice l
    withChance chance (Just c) Nothing

repM :: (Integral a, MonadIO m) => a -> [b] -> m [b]
repM n l = replicateM (fromIntegral n) (choice l)

