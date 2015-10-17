module Main (main) where

import qualified MoodleBench
-- HASKELETON: import qualified New.ModuleBench

import Criterion.Main (bgroup, defaultMain)

main :: IO ()
main = defaultMain
    [ bgroup "Moodle" MoodleBench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]
