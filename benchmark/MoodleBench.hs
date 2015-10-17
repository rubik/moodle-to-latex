module MoodleBench (benchmarks) where

import Moodle

import Criterion

benchmarks :: [Benchmark]
benchmarks =
    [ bench "main" (nfIO main)
    ]
